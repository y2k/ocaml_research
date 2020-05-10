type event = TodoCreated of string | TodoRemoved of string

type serialized_event = {key: string; data: bytes}

let memory_reducer (_ : event) (_ : 'a) : 'a = failwith "???"

let disk_reducer key e : unit Lwt.t =
  let _se = {key; data= Marshal.to_bytes e []} in
  failwith "???"

type db = {count: int; last_todos: string list; filter: string * string list}

let filter_reducer e db =
  let contains s1 s2 =
    let re = Str.regexp_string s2 in
    try
      ignore (Str.search_forward re s1 0) ;
      true
    with Not_found -> false
  in
  match e with
  | TodoCreated x ->
      if contains x (fst db.filter) then
        {db with filter= (fst db.filter, x :: snd db.filter)}
      else db
  | TodoRemoved x ->
      { db with
        filter= (fst db.filter, snd db.filter |> List.filter (fun y -> x <> y))
      }

let count_reducer (e : event) db =
  match e with
  | TodoCreated _ ->
      {db with count= db.count + 1}
  | TodoRemoved _ ->
      {db with count= db.count - 1}

let last_top (e : event) db =
  match e with
  | TodoCreated x ->
      let rec take n xs =
        match n with 0 -> [] | _ -> List.hd xs :: take (n - 1) xs
      in
      if List.length db.last_todos >= 20 then
        {db with last_todos= x :: take 20 db.last_todos}
      else {db with last_todos= x :: db.last_todos}
  | TodoRemoved x ->
      {db with last_todos= List.filter (fun y -> y <> x) db.last_todos}

let main_reduce e db = db |> filter_reducer e |> count_reducer e |> last_top e
