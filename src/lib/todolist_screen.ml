module Update = struct
  open Storage

  type model = {items: string list; text: string; enabled: bool}

  type msg =
    | Create
    | ShowNotification
    | ShowToast
    | Delete of int
    | UpdateText of string
    | StoreUpdated of TodoStoreReducer.store

  module Serializer = struct
    let serialize = function
      | UpdateText x ->
          `Assoc [("u", `String x)]
      | Delete id ->
          `Assoc [("d", `Int id)]
      | Create ->
          `Assoc [("c", `Null)]
      | ShowNotification ->
          `Assoc [("sn", `Null)]
      | ShowToast ->
          `Assoc [("st", `Null)]
      | _ ->
          failwith @@ fst @@ __LOC_OF__ ()

    let deserialize = function
      | `Assoc [("u", `String x)] ->
          UpdateText x
      | `Assoc [("d", `Int x)] ->
          Delete x
      | `Assoc [("c", `Null)] ->
          Create
      | `Assoc [("sn", `Null)] ->
          ShowNotification
      | `Assoc [("st", `Null)] ->
          ShowToast
      | _ ->
          failwith @@ fst @@ __LOC_OF__ ()
  end

  let sub (db : TodoStoreReducer.store) (model : model) : model =
    {model with items= db.todos}

  let init dispatch =
    ( {items= []; text= ""; enabled= false}
    , [ `SendEvent
          ( TodoStoreReducer.TodoInvalidated
          , fun db -> StoreUpdated db |> dispatch ) ] )

  let update dispatch model msg =
    match msg with
    | StoreUpdated db ->
        (sub db model, [])
    | Create ->
        ( {model with text= ""; enabled= false}
        , [ `SendEvent
              ( TodoStoreReducer.TodoCreated model.text
              , fun db -> StoreUpdated db |> dispatch ) ] )
    | ShowNotification ->
        (model, [`ShowNotification "hello from OCaml"])
    | ShowToast ->
        (model, [`ShowToast "hello from OCaml"])
    | Delete index ->
        let todo = List.nth model.items index in
        ( model
        , [ `SendEvent
              ( TodoStoreReducer.TodoRemoved todo
              , fun db -> StoreUpdated db |> dispatch ) ] )
    | UpdateText text ->
        ({model with text; enabled= String.length text > 0}, [])
end

module View = struct
  open Dsl
  open Printf
  open Update
  module M = Dsl.Material

  let viewItem dispatch i x =
    M.list_item [("hasmeta", "")]
      [ span [] [text @@ sprintf "Item #%s" x]
      ; span
          [ ("slot", "meta")
          ; cls "material-icons"
          ; ("onclick", dispatch @@ Delete i) ]
          [text "delete"] ]

  let viewButton ?(enabled = true) title onclick =
    M.button
      [ ("label", title)
      ; ("style", "margin: 4px")
      ; ("raised", "")
      ; ((if enabled then "enabled" else "disabled"), "")
      ; ("onclick", onclick) ]

  let view dispatch (model : Update.model) =
    div
      [("style", "display: flex; flex-direction: column")]
      [ M.textfield
          [ ("label", "Enter todo item")
          ; ("value", model.text)
          ; ("oninput", dispatch @@ UpdateText value_source) ]
      ; h4 [("style", "margin: 8px")] [text @@ sprintf "Add item: %s" model.text]
      ; viewButton "show notification" (dispatch ShowNotification)
      ; viewButton "show toast" (dispatch ShowToast)
      ; viewButton ~enabled:model.enabled
          (sprintf "Add (%i)" (List.length model.items))
          (dispatch Create)
      ; M.list [] (model.items |> List.mapi (viewItem dispatch)) ]
end
