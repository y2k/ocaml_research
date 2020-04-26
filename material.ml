module Update = struct
  type model = {items: string list; text: string; enabled: bool}

  type msg =
    | Create
    | ShowNotification
    | ShowToast
    | Delete of int
    | UpdateText of string

  let init = ({items= []; text= ""; enabled= false}, [])

  let update model msg =
    match msg with
    | Create ->
        ({items= model.text :: model.items; text= ""; enabled= false}, [])
    | ShowNotification ->
        (model, [`ShowNotification "hello from OCaml"])
    | ShowToast ->
        (model, [`ShowToast "hello from OCaml"])
    | Delete index ->
        model.items
        |> List.mapi (fun i x -> if i = index then None else Some x)
        |> List.filter_map Fun.id
        |> fun items -> ({model with items}, [])
    | UpdateText text ->
        ({model with text; enabled= String.length text > 0}, [])
end

module View = struct
  open Dsl
  open Printf
  open Update
  module M = Dsl.Material
  module D = Remote.Dispatch

  let viewItem i x =
    M.list_item [("hasmeta", "")]
      [ span [] [text @@ sprintf "Item #%s" x]
      ; span
          [ ("slot", "meta")
          ; cls "material-icons"
          ; ("onclick", D.dispatch @@ Delete i) ]
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
          ; viewButton "show notification" (D.dispatch ShowNotification)
          ; viewButton "show toast" (D.dispatch ShowToast)
          ; viewButton ~enabled:model.enabled
              (sprintf "Add (%i)" (List.length model.items))
              (D.dispatch Create)
          ; M.list [] (model.items |> List.mapi viewItem) ] ]
end
