type event = TodoCreated of string | TodoRemoved of string | Reseted

module Domain = struct
  let reduce_filter e (_ : int) (filter : string * string list) =
    let contains s1 s2 =
      let re = Str.regexp_string s2 in
      try
        ignore (Str.search_forward re s1 0) ;
        true
      with Not_found -> false
    in
    match e with
    | TodoCreated x ->
        if contains x (fst filter) then x :: snd filter else snd filter
    | TodoRemoved x ->
        snd filter |> List.filter (fun y -> x <> y)
    | Reseted ->
        []

  let reduce_count (e : event) count =
    match e with
    | TodoCreated _ ->
        count + 1
    | TodoRemoved _ ->
        count - 1
    | Reseted ->
        0

  let reduce_last_todos (e : event) last_todos =
    match e with
    | TodoCreated x ->
        let rec take n xs =
          match n with 0 -> [] | _ -> List.hd xs :: take (n - 1) xs
        in
        if List.length last_todos >= 20 then x :: take 20 last_todos
        else x :: last_todos
    | TodoRemoved x ->
        List.filter (fun y -> y <> x) last_todos
    | Reseted ->
        []
end

module MainScreen = struct
  type model = {last_todos: string list; count: int; text: string}

  type msg = TextChanged of string | CreateTodoPressed

  let init : model = {last_todos= []; count= 0; text= ""}

  let sub (e : event) (m : model) : model =
    { m with
      last_todos= Domain.reduce_last_todos e m.last_todos
    ; count= Domain.reduce_count e m.count }

  let update (m : model) (msg : msg) =
    match msg with
    | TextChanged t ->
        ({m with text= t}, [])
    | CreateTodoPressed ->
        (m, [`SendEvent (TodoCreated m.text)])
end

module SearchScreen = struct
  type model =
    {filter: string; todos: string list; inner_filter: string; page: int}

  type msg = FilterChanged of string | ApplyFilterPressed

  let init = {filter= ""; todos= []; inner_filter= ""; page= 0}

  let sub (e : event) (m : model) =
    {m with todos= Domain.reduce_filter e m.page (m.inner_filter, m.todos)}

  let update (m : model) (msg : msg) =
    match msg with
    | FilterChanged f ->
        ({m with filter= f}, [])
    | ApplyFilterPressed ->
        ({m with inner_filter= m.filter; todos= []; page= 0}, [`ResetEvents])
end
