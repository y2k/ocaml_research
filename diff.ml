type complex_id = int list

type diff_action =
  | AddNode of complex_id * complex_id * string
  | RemoveNode of complex_id
  | SetProp of complex_id * string * string
  | RemoveProp of complex_id * string

type node = Dsl.node

module List = struct
  include List

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

let rec compute_diff (prevs : node list) (currents : node list)
    (parent_id : complex_id) =
  List.zip prevs currents
  |> List.mapi (fun i x ->
         match x with
         | Some _, None ->
             [RemoveNode (i :: parent_id)]
         | None, Some (current : node) ->
             [AddNode (parent_id, i :: parent_id, current.tag)]
             @ set_new_props [] current (i :: parent_id)
             @ compute_diff [] current.children (i :: parent_id)
         | Some (prev : node), Some (current : node) ->
             if prev.tag = current.tag then
               set_new_props prev.props current (i :: parent_id)
               @ remove_old_props prev current (i :: parent_id)
               @ compute_diff prev.children current.children (i :: parent_id)
             else
               [ RemoveNode (i :: parent_id)
               ; AddNode (parent_id, i :: parent_id, current.tag) ]
               @ set_new_props [] current (i :: parent_id)
               @ compute_diff [] current.children (i :: parent_id)
         | _ ->
             failwith "illegal state")
  |> List.concat

module Renderer = struct
  open Dsl

  let render =
    html []
      [ head []
          [ meta
              [ ("name", "viewport")
              ; ( "content"
                , "minimum-scale=1, initial-scale=1, width=device-width" ) ]
          ; link
              [ ("rel", "stylesheet")
              ; ( "href"
                , "https://fonts.googleapis.com/css?family=Material+Icons&display=block"
                ) ]
          ; link
              [ ("rel", "stylesheet")
              ; ( "href"
                , "https://fonts.googleapis.com/css?family=Roboto:300,400,500"
                ) ]
          ; script [("src", "bundle.js")] [] ]
      ; body [("style", "margin: 0px")]
          [ div [("id", "container")] []
          ; script []
              [ text
                  {|
                         (function() {
                           remote_ui_ws = new WebSocket('ws://' + window.location.hostname +':8081/');
                           remote_ui_ws.onmessage = function(msg) {
                             const cmds = JSON.parse(msg.data);
                             for (cmd of cmds) {
                               switch (cmd.tag) {
                                 case "add-node":
                                   if (cmd.name != "") {
                                     const node = document.createElement(cmd.name);
                                     node.id = cmd.id;
                                     document.getElementById(cmd.parent_id || "container").appendChild(node);
                                   }
                                   break;
                                 case "remove-node":
                                   document.getElementById(cmd.id).remove();
                                   break;
                                 case "set-prop":
                                   if (cmd.name == "") {
                                     document.getElementById(cmd.id.substring(2) || "container").innerText = cmd.value;
                                   } else {
                                     document.getElementById(cmd.id).setAttribute(cmd.name, cmd.value)
                                   }
                                   break;
                                 case "remove-prop":
                                   document.getElementById(cmd.id).removeAttribute(cmd.name)
                                   break;
                               }
                             }
                           };
                         })();
                       |}
              ] ] ]
end
