open Prelude

module TodoStoreReducer = struct
  type event =
    | TodoCreated of string
    | TodoRemoved of string
    | TodoInvalidated
    | CityFavorited of string
    | CityUnfavorited of string
    | UpdateFeed of Feed_domain.post list

  type store =
    { todos: string list
    ; favorite_cities: string list
    ; feed: Feed_domain.post list }

  let empty_store = {todos= []; favorite_cities= []; feed= []}

  let reduce_to_memory (e : event) (db : store) =
    match e with
    | TodoInvalidated ->
        db
    | UpdateFeed xs ->
        {db with feed= xs}
    | TodoCreated x ->
        {db with todos= x :: db.todos}
    | TodoRemoved x ->
        {db with todos= db.todos |> List.filter (( <> ) x)}
    | CityFavorited x ->
        {db with favorite_cities= x :: db.favorite_cities}
    | CityUnfavorited x ->
        {db with favorite_cities= db.favorite_cities |> List.filter (( <> ) x)}

  let reduce_to_disk (e : event) =
    match e with
    | TodoInvalidated ->
        ("SELECT 1", [])
    | UpdateFeed posts ->
        let open Yojson.Safe in
        let json =
          posts
          |> List.map Feed_domain.post_to_yojson
          |> fun x -> `List x |> to_string
        in
        ("DELETE FROM feed_cache; INSERT INTO feed_cache VALUES (?)", [json])
    | TodoCreated x ->
        ("INSERT INTO todos VALUES (?)", [x])
    | TodoRemoved x ->
        ("DELETE FROM todos WHERE value = ?", [x])
    | CityFavorited x ->
        ("INSERT INTO favorite_cities VALUES (?)", [x])
    | CityUnfavorited x ->
        ("DELETE FROM favorite_cities WHERE value = ?", [x])

  let restore_from_disk =
    [ ( "CREATE TABLE IF NOT EXISTS todos (value TEXT); SELECT * FROM todos"
      , fun row db -> {db with todos= row.(0) :: db.todos} )
    ; ( "CREATE TABLE IF NOT EXISTS favorite_cities (value TEXT); SELECT * \
         FROM favorite_cities"
      , fun row db -> {db with favorite_cities= row.(0) :: db.favorite_cities}
      )
    ; ( "CREATE TABLE IF NOT EXISTS feed_cache (value TEXT); SELECT * FROM \
         feed_cache"
      , fun row db ->
          let open Yojson.Safe in
          let feed =
            from_string row.(0)
            |> Util.convert_each Feed_domain.post_of_yojson
            |> List.map (function Ok x -> x | Error e -> fail @@ __LOC_OF__ e)
          in
          {db with feed} ) ]
end

module type StoreReducerSig = sig
  type event

  type store

  val empty_store : store

  val reduce_to_memory : event -> store -> store

  val reduce_to_disk : event -> string * string list

  val restore_from_disk : (string * (string array -> store -> store)) list
end

module PersistentStore (F : StoreReducerSig) : sig
  val init : string -> F.event -> F.store
end = struct
  open Sqlite3

  let restore db (store : F.store) =
    let restore' store (sql, reduce) =
      let sr = ref store in
      ignore
        (exec_not_null_no_headers db sql ~cb:(fun row ->
             let s = !sr in
             sr := reduce row s)) ;
      !sr
    in
    F.restore_from_disk |> List.fold_left restore' store

  let init db_name =
    let store = ref F.empty_store in
    let db = db_open db_name in
    store := restore db !store ;
    fun (e : F.event) ->
      store := F.reduce_to_memory e !store ;
      let sql, params = F.reduce_to_disk e in
      let stmt = prepare db sql in
      ignore (reset stmt) ;
      params |> List.iteri (fun i x -> ignore (bind stmt (i + 1) (Data.TEXT x))) ;
      ignore (step stmt) ;
      ignore (finalize stmt) ;
      !store
end

module TodoStore = PersistentStore (TodoStoreReducer)
