module Domain = struct
  let toCommand text =
    let find key xs = xs |> List.assoc_opt key |> Option.map List.hd in
    let parts = Uri.query_of_encoded text in
    find "id" parts
    |> Fun.flip Option.bind (fun id ->
           find "value" parts |> Option.map (fun value -> (id, value)))
    |> Option.fold ~none:("", "") ~some:Fun.id

  let remove xs v = xs |> List.filter (fun x -> x <> v)
end

module Update = struct
  type model = {todos: string list}

  let init = ({todos= []}, [])

  type msg = ServerUpdate of string
  type cmd = TODO

  let update model msg =
    match msg with
    | ServerUpdate body -> (
      match Domain.toCommand body with
      | "enter_text", value -> ({todos= value :: model.todos}, [])
      | "delete", value -> ({todos= Domain.remove model.todos value}, [])
      | _ -> (model, []) )
end

module View = struct
  open Dsl

  let item title =
    div [("style", "margin: 8px")]
      [ div [cls "card"]
          [ div [cls "card-content"] [div [cls "content"] [text title]]
          ; footer [cls "card-footer"]
              [a [("href", "#"); cls "card-footer-item"] [text "Delete"]] ] ]

  let add_item =
    form
      [cls "field has-addons"; ("method", "post"); ("style", "flex: 1")]
      [ p
          [cls "control"; ("style", "flex: 1")]
          [ input
              [ cls "input"; ("type", "text"); ("placeholder", "Enter item")
              ; ("name", "value") ]
              [] ]
      ; p [cls "control"]
          [ button
              [cls "button"; ("name", "id"); ("value", "enter_text")]
              [text "Add"] ] ]

  let view (model : Update.model) =
    div []
      [ div [cls "navbar is-primary"]
          [ div [cls "navbar-item"]
              [p [cls "is-size-4 has-text-white"] [text "Todo List"]] ]
      ; section [cls "hero"]
          [ div [cls "panel-block"] [add_item]
          ; div [] (model.todos |> List.map item) ] ]
    |> scaffold
end
