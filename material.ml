module Update = struct
  type model = {items: string list; text: string; enabled: bool}

  type msg = ServerUpdate of string

  let init = ({items= []; text= ""; enabled= false}, [])

  let update model msg =
    match msg with
    | ServerUpdate "_create_" ->
        ({items= model.text :: model.items; text= ""; enabled= false}, [])
    | ServerUpdate "_show_notification_" ->
        (model, [`ShowNotification "hello from OCaml"])
    | ServerUpdate "_show_toast_" ->
        (model, [`ShowToast "hello from OCaml"])
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
  module M = Dsl.Material

  let viewItem i x =
    M.list_item [("hasmeta", "")]
      [ span [] [text @@ sprintf "Item #%s" x]
      ; span
          [ ("slot", "meta")
          ; cls "material-icons"
          ; ("onclick", sprintf "remote_ui_ws.send('_delete_%i')" i) ]
          [text "delete"] ]

  let viewButton ?(enabled = true) title onclick =
    M.button
      [ ("label", title)
      ; ("style", "margin: 4px")
      ; ("raised", "")
      ; ((if enabled then "enabled" else "disabled"), "")
      ; ("onclick", onclick) ]

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
          ; viewButton "show notification"
              "remote_ui_ws.send('_show_notification_')"
          ; viewButton "show toast" "remote_ui_ws.send('_show_toast_')"
          ; viewButton ~enabled:model.enabled
              (sprintf "Add (%i)" (List.length model.items))
              "remote_ui_ws.send('_create_')"
          ; M.list [] (model.items |> List.mapi viewItem) ] ]
end
