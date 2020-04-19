type complex_id = int list

type diff_action =
  | AddNode of complex_id * complex_id * string
  | RemoveNode of complex_id
  | SetProp of complex_id * string * string
  | RemoveProp of complex_id * string

type node = Dsl.node

module ListExt = struct
  let zip xs ys =
    let count = max (List.length xs) (List.length ys) in
    List.init count (fun i -> (List.nth_opt xs i, List.nth_opt ys i))
end

let set_new_props (prevProps : (string * string) list) (current : node)
    (id : complex_id) =
  current.props
  |> List.map (fun (k, v) ->
         match prevProps |> List.find_opt (fun (k', _) -> k = k') with
         | None ->
             [SetProp (id, k, v)]
         | Some (_, v') when v <> v' ->
             [SetProp (id, k, v)]
         | _ ->
             [])
  |> List.concat

let remove_old_props (prev : node) (current : node) (id : complex_id) =
  prev.props
  |> List.map (fun (k, _) ->
         match current.props |> List.find_opt (fun (k', _) -> k = k') with
         | None ->
             [RemoveProp (id, k)]
         | _ ->
             [])
  |> List.concat

let rec compute_childred (prevs : node list) (currents : node list)
    (parent_id : complex_id) =
  ListExt.zip prevs currents
  |> List.mapi (fun i x ->
         match x with
         | Some _, None ->
             [RemoveNode (i :: parent_id)]
         | None, Some (current : node) ->
             [AddNode (parent_id, i :: parent_id, current.tag)]
             @ set_new_props [] current (i :: parent_id)
             @ compute_childred [] current.children parent_id
         | Some (prev : node), Some (current : node) ->
             if prev.tag = current.tag then
               set_new_props prev.props current (i :: parent_id)
               @ remove_old_props prev current (i :: parent_id)
               @ compute_childred prev.children current.children parent_id
             else
               [ RemoveNode (i :: parent_id)
               ; AddNode (parent_id, i :: parent_id, current.tag) ]
               @ set_new_props [] current (i :: parent_id)
               @ compute_childred [] current.children parent_id
         | _ ->
             failwith "illegal state")
  |> List.concat
