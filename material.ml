module MaterialDsl = struct
  open Dsl

  let textfield props = {tag= "mwc-textfield"; props; children= []}

  let list_item props children = {tag= "mwc-list-item"; props; children}

  let list props children = {tag= "mwc-list"; props; children}

  let button props = {tag= "mwc-button"; props; children= []}

  let icon_button props = {tag= "mwc-icon-button"; props; children= []}

  let top_app_bar props children = {tag= "mwc-top-app-bar"; props; children}
end

module Update = struct
  type model = {items: string list; text: string; enabled: bool}

  type msg = ServerUpdate of string

  let init = ({items= []; text= ""; enabled= false}, [])

  let update model msg =
    match msg with
    | ServerUpdate "_create_" ->
        ({items= model.text :: model.items; text= ""; enabled= false}, [])
    | ServerUpdate x
      when Str.string_match (Str.regexp {|_delete_\([0-9]+\)|}) x 0 ->
        let index = int_of_string @@ Str.matched_group 1 x in
        model.items
        |> List.mapi (fun i x -> if i = index then None else Some x)
        |> List.filter_map Fun.id
        |> fun items -> ({model with items}, [])
    | ServerUpdate text ->
        ({model with text; enabled= String.length text > 0}, [])
end

module View = struct
  open Dsl
  open Printf
  module M = MaterialDsl

  let viewItem i x =
    M.list_item [("hasmeta", "")]
      [ span [] [text @@ sprintf "Item #%s" x]
      ; span
          [ ("slot", "meta")
          ; cls "material-icons"
          ; ("onclick", sprintf "remote_ui_ws.send('_delete_%i')" i) ]
          [text "delete"] ]

  let view (model : Update.model) =
    M.top_app_bar []
      [ M.icon_button [("icon", "menu"); ("slot", "navigationIcon")]
      ; div [("slot", "title")] [text "Todo List"]
      ; div
          [("style", "display: flex; flex-direction: column")]
          [ M.textfield
              [ ("label", "Enter todo item")
              ; ("value", model.text)
              ; ("oninput", "remote_ui_ws.send(this.value)") ]
          ; h4 [("style", "margin: 8px")]
              [text @@ sprintf "Add item: %s" model.text]
          ; M.button
              [ ("label", sprintf "Add (%i)" (List.length model.items))
              ; ("raised", "")
              ; ((if model.enabled then "enabled" else "disabled"), "")
              ; ("onclick", "remote_ui_ws.send('_create_')") ]
          ; M.list [] (model.items |> List.mapi viewItem) ] ]

  let render_static =
    html []
      [ head []
          [ link
              [ ("rel", "stylesheet")
              ; ( "href"
                , "https://fonts.googleapis.com/css?family=Material+Icons&display=block"
                ) ]
          ; link
              [ ("rel", "stylesheet")
              ; ( "href"
                , "https://fonts.googleapis.com/css?family=Roboto:300,400,500"
                ) ]
          ; script
              [ ( "src"
                , "https://cdnjs.cloudflare.com/ajax/libs/webcomponentsjs/2.4.3/webcomponents-bundle.js"
                ) ]
              []
          ; script
              [ ("type", "module")
              ; ( "src"
                , "https://mwc-demos.glitch.me/node_modules/@material/mwc-button/mwc-button.js"
                ) ]
              []
          ; script
              [ ("type", "module")
              ; ( "src"
                , "https://mwc-demos.glitch.me/node_modules/@material/mwc-icon/mwc-icon.js"
                ) ]
              []
          ; script
              [ ("type", "module")
              ; ( "src"
                , "https://mwc-demos.glitch.me/node_modules/@material/mwc-icon-button/mwc-icon-button.js"
                ) ]
              []
          ; script
              [ ("type", "module")
              ; ( "src"
                , "https://mwc-demos.glitch.me/node_modules/@material/mwc-top-app-bar/mwc-top-app-bar.js"
                ) ]
              []
          ; script
              [ ("type", "module")
              ; ( "src"
                , "https://mwc-demos.glitch.me/node_modules/@material/mwc-list/mwc-list.js"
                ) ]
              []
          ; script
              [ ("type", "module")
              ; ( "src"
                , "https://mwc-demos.glitch.me/node_modules/@material/mwc-list/mwc-list-item.js"
                ) ]
              []
          ; script
              [ ("type", "module")
              ; ( "src"
                , "https://mwc-demos.glitch.me/node_modules/@material/mwc-textfield/mwc-textfield.js"
                ) ]
              [] ]
      ; body [("style", "margin: 0px")]
          [ div [("id", "container")] []
          ; script []
              [ text
                  {|
                         (function() {
                           remote_ui_ws = new WebSocket('ws://localhost:8081/');
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
