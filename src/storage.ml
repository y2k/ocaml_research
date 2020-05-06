module VirtualMap = struct
  type t = {collection: string; version: int option}

  type 'a m = {db: t}

  let last_db : t = failwith "???"

  let create (_name : string) : t Lwt.t = failwith "???"

  let count (_ : t) : int m = failwith "???"

  let paged (_ : t) (_page_size : int) : 'a list m = failwith "???"

  let get_otp (_ : 'a m) : 'a option = failwith "???"

  let get (_ : 'a m) : 'a = failwith "???"

  let insert (_ : 'a) : 'b Lwt.t = failwith "???"

  let map (_ : 'a -> 'b) (_ : 'a m) : 'b m = failwith "???"

  let set_offset (_ : int) (_ : 'a list m) : 'a list m = failwith "???"

  let move_offset (_ : int) (_ : 'a list m) : 'a list m = failwith "???"
end

module MainScreen = struct
  open Dsl
  module V = VirtualMap

  type todo = {text: string}

  type model = {count: int V.m; last_todos: todo list V.m; text: string}

  type msg = TextChanged of string | Create | InsertEnd of V.t

  let init =
    let db = V.last_db in
    {count= V.count db; last_todos= V.paged db 20; text= ""}

  let update (model : model) (msg : msg) =
    match msg with
    | TextChanged x ->
        ({model with text= x}, [])
    | Create ->
        let todo = {text= model.text} in
        ( {model with text= ""}
        , [V.insert todo |> Lwt.map (fun x -> InsertEnd x)] )
    | InsertEnd db ->
        ({model with count= V.count db; last_todos= V.paged db 20}, [])

  let view model dispatch =
    div []
      [ input [("onchanged", dispatch @@ TextChanged "{value}")] []
      ; button [("onclick", dispatch @@ Create)] [text "add"]
      ; div []
          ( V.get model.last_todos
          |> List.map (fun (x : todo) -> div [] [text x.text]) )
      ; span [] [text ("Count: " ^ string_of_int @@ V.get model.count)] ]
end

module HistoryScreen = struct
  open Dsl
  module V = VirtualMap

  let page_size = 20

  type todo = {text: string}

  type model = {todos: todo list V.m; page_count: int V.m; page: int}

  type msg = Next

  let init =
    { todos= V.paged V.last_db page_size
    ; page_count= V.count V.last_db |> V.map (fun x -> x / page_size)
    ; page= 0 }

  let update model msg =
    match msg with
    | Next ->
        let new_page = model.page + 1 in
        ( { model with
            page= new_page
          ; todos= model.todos |> V.set_offset (new_page * page_size) }
        , [] )

  let view model dispatch =
    div []
      [ div []
          ( V.get model.todos
          |> List.map (fun (x : todo) -> div [] [text x.text]) )
      ; div []
          [ span [] [text "0 .. "]
          ; span [] [text @@ string_of_int model.page]
          ; span [] [text @@ " .. " ^ string_of_int @@ V.get model.page_count]
          ; button [("onclick", dispatch Next)] [text "next"] ] ]
end

module Database = struct end

type collection = Collection of string

type key = Key of string

type event = Insert of collection * key * bytes | Delete of collection * key

type cursor = {offset: int; filter: string; order: string option; version: int}

type query =
  | SelectAllPaged of cursor
  | SelectCount of cursor * int option
  | SelectLastOrderedDesc of cursor

module MkStore (T : sig
  type t

  val default : t

  val diff : t -> t -> event list

  val reduce : t -> event -> t

  val queries : t -> (string * query) list
end) =
struct
  module D = Database

  let store = ref T.default

  let readAll (_ : event -> unit) : unit = failwith "???"

  let saveDiff (_ : event list) : unit = failwith "???"

  let init () = readAll (fun e -> store := T.reduce !store e)

  let update (f : T.t -> T.t * 'a) =
    let new_store, result = f !store in
    saveDiff (T.diff !store new_store) ;
    store := new_store ;
    result
end

module Types = struct
  type todo = Todo of string * float

  type model = {todos: todo list; top_todos: todo list; total_count: int}
end

module type UserSession = sig
  val user_id : string
end

type user_id = UserId of string

module Tests (U : UserSession) = struct
  open Types

  module Db = MkStore (struct
    type t = model

    let default = {todos= []; top_todos= []; total_count= 0}

    module Utils = struct
      let diff_added (_ : 'a list) (_ : 'a list) : 'a list = failwith "???"

      let diff_removed (_ : 'a list) (_ : 'a list) : 'a list = failwith "???"

      let compute_digest (_ : 'a) : string = failwith "???"
    end

    let queries _ =
      [ ( "all-count"
        , SelectCount ({filter= "*"; offset= 0; order= None; version= 0}, None)
        )
      ; ( "last-todos"
        , SelectAllPaged
            {filter= "*"; offset= 20; order= Some "[created][desc]"; version= 0}
        ) ]

    let diff (db : model) (new_db : model) =
      let collection = Collection (U.user_id ^ "_todos") in
      let added =
        Utils.diff_added db.todos new_db.todos
        |> List.map (fun x ->
               Insert
                 ( collection
                 , Key (Utils.compute_digest x)
                 , Marshal.to_bytes x [] ))
      in
      let removed =
        Utils.diff_removed db.todos new_db.todos
        |> List.map (fun x -> Delete (collection, Key (Utils.compute_digest x)))
      in
      added @ removed

    let reduce (_db : model) (_event : event) = failwith "???"
  end)

  let add_new (text : string) =
    let update db = {db with todos= Todo (text, Sys.time ()) :: db.todos} in
    Db.update (fun db -> (update db, ()))

  let todo_list _user_id =
    let filter (db : model) = db.todos in
    Db.update (fun db -> (db, filter db))

  let execute_query (q : query) : query =
    let find_in_cache (_ : query) : 'a option = failwith "???" in
    let load_from_disk (_ : query) : 'a = failwith "???" in
    let save_to_cache (_ : query) _ : unit = failwith "???" in
    match find_in_cache q with
    | Some r ->
        r
    | None ->
        let r = load_from_disk q in
        save_to_cache q r ; r

  let count () =
    let q =
      SelectCount
        ( {filter= "collection = 'todos'"; offset= 0; order= None; version= 0}
        , None )
    in
    match execute_query q with
    | SelectCount (_, Some count) ->
        count
    | _ ->
        failwith "???"
end
