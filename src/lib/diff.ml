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
  let update_node (prev : node) (current : node) i parent_id =
    if prev.tag = current.tag then
      set_new_props prev.props current (i :: parent_id)
      @ remove_old_props prev current (i :: parent_id)
      @ compute_diff prev.children current.children (i :: parent_id)
    else
      [ RemoveNode (i :: parent_id)
      ; AddNode (parent_id, i :: parent_id, current.tag) ]
      @ set_new_props [] current (i :: parent_id)
      @ compute_diff [] current.children (i :: parent_id)
  in
  List.zip prevs currents
  |> List.mapi (fun i x ->
         match x with
         | Some _, None ->
             [RemoveNode (i :: parent_id)]
         | None, Some (current : node) -> (
           match current.lazyNode with
           | Some lazy_node ->
               let current = Lazy.force lazy_node.view in
               [AddNode (parent_id, i :: parent_id, current.tag)]
               @ set_new_props [] current (i :: parent_id)
               @ compute_diff [] current.children (i :: parent_id)
           | None ->
               [AddNode (parent_id, i :: parent_id, current.tag)]
               @ set_new_props [] current (i :: parent_id)
               @ compute_diff [] current.children (i :: parent_id) )
         | Some (prev : node), Some (current : node) -> (
           match current.lazyNode with
           | Some current_lazy -> (
             match prev.lazyNode with
             | Some prev_lazy ->
                 if current_lazy.token = prev_lazy.token then []
                 else
                   let current = Lazy.force current_lazy.view in
                   let prev = Lazy.force prev_lazy.view in
                   update_node prev current i parent_id
             | None ->
                 let current = Lazy.force current_lazy.view in
                 [ RemoveNode (i :: parent_id)
                 ; AddNode (parent_id, i :: parent_id, current.tag) ]
                 @ set_new_props [] current (i :: parent_id)
                 @ compute_diff [] current.children (i :: parent_id) )
           | None -> (
             match prev.lazyNode with
             | Some prev_lazy ->
                 let prev = Lazy.force prev_lazy.view in
                 update_node prev current i parent_id
             | None ->
                 update_node prev current i parent_id ) )
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
  function reconect() {
    remote_ui_ws = new WebSocket('ws://' + window.location.hostname + ':8081/');
    remote_ui_ws.onclose = function(event) { reconectWithDelay(); };
    let isFirstMessage = true;
    remote_ui_ws.onmessage = function(msg) {
      if (isFirstMessage) {
        document.getElementById("container").innerHTML = "";
        isFirstMessage = false;
      }
      const cmds = JSON.parse(msg.data);
      for (cmd of cmds) {
        switch (cmd.t) {
          case "a":
            if (cmd.n != "") {
              const node = document.createElement(cmd.n);
              node.id = cmd.i;
              document.getElementById(cmd.p || "container").appendChild(node);
            }
            break;
          case "r":
            document.getElementById(cmd.i).remove();
            break;
          case "s":
            if (cmd.n == "") {
              document.getElementById(cmd.i.substring(2) || "container").innerText = cmd.v;
            } else {
              document.getElementById(cmd.i).setAttribute(cmd.n, cmd.v)
            }
            break;
          case "d":
            document.getElementById(cmd.i).removeAttribute(cmd.n)
            break;
        }
      }
    };
  };
  function reconectWithDelay() { 
    setTimeout(function () { reconect() }, 2000) 
  }
  reconect();
})();
                       |}
              ] ] ]
end

module LazyView = struct
  open Dsl

  let view (model : 'a) (view : 'a -> node) : node =
    let ln = {token= Marshal.to_bytes model []; view= lazy (view model)} in
    {tag= "__LAZY__"; props= []; children= []; lazyNode= Some ln}
end
