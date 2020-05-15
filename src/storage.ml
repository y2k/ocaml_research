module TodoStoreReducer = struct
  type event = TodoCreated of string | TodoRemoved of string | TodoInvalidated

  type store = {todos: string list}

  let empty_store = {todos= []}

  let reduce_to_memory (e : event) (db : store) =
    match e with
    | TodoCreated x ->
        {todos= x :: db.todos}
    | TodoRemoved x ->
        {todos= db.todos |> List.filter (fun y -> x <> y)}
    | TodoInvalidated ->
        db

  let reduce_to_disk (e : event) =
    match e with
    | TodoCreated x ->
        ("INSERT INTO todos VALUES (?)", [x])
    | TodoRemoved x ->
        ("DELETE FROM todos WHERE value = ?", [x])
    | TodoInvalidated ->
        ("SELECT 1", [])

  let restore_from_disk : string list * (string array -> store -> store) =
    ( ["CREATE TABLE IF NOT EXISTS todos (value TEXT)"; "SELECT * FROM todos"]
    , fun row s -> {todos= row.(0) :: s.todos} )
end

module type StoreReducerSig = sig
  type event

  type store

  val empty_store : store

  val reduce_to_memory : event -> store -> store

  val reduce_to_disk : event -> string * string list

  val restore_from_disk : string list * (string array -> store -> store)
end

module PersistentStore (F : StoreReducerSig) : sig
  val init : string -> F.event -> F.store
end = struct
  open Sqlite3

  let restore db (store : F.store) =
    let sqls, reduce = F.restore_from_disk in
    let sr = ref store in
    sqls
    |> List.iter (fun sql ->
           ignore
             (exec_not_null_no_headers db sql ~cb:(fun row ->
                  let s = !sr in
                  sr := reduce row s))) ;
    !sr

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
