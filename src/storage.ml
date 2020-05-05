module Database = struct end

type collection = Collection of string

type key = Key of string

type event = Insert of collection * key * bytes | Delete of collection * key

type cursor = {offset: int; filter: string; order: string; version: int}

type query =
  | SelectAllPaged of cursor
  | SelectCount of cursor
  | SelectLastOrderedDesc of cursor

module MkStore (T : sig
  type t

  val default : t

  val diff : t -> t -> event list

  val reduce : t -> event -> t

  val queries : query list
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

    let queries = [SelectCount {filter= "*"; offset= 0; order= ""; version= 0}]

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
end
