module Domain = struct
  let toCommand text =
    let find_value key =
      let r = Str.regexp (".*" ^ key ^ {|=\([^&]+\).*|}) in
      if Str.string_match r text 0 then Some (Str.matched_group 1 text)
      else None in
    find_value "id"
    |> Fun.flip Option.bind (fun id ->
           find_value "value" |> Option.map (fun value -> (id, value)))
    |> Option.fold ~none:("", "") ~some:Fun.id

  let isUrl text =
    let r = Str.regexp {|\(https://\|http://\).+|} in
    Str.string_match r text 0

  let getTitle html =
    let r = Str.regexp {|.+<title>\(.+?\)</title>.+|} in
    if Str.string_match r html 0 then Some (Str.matched_group 1 html) else None

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

  let scaffold node =
    html []
      [ head []
          [ title [] [text "OCaml Research"]; meta [("charset", "utf-8")]
          ; meta
              [ ("name", "viewport")
              ; ("content", "width=device-width, initial-scale=1") ]
          ; link
              [ ("rel", "stylesheet")
              ; ( "href"
                , "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css"
                ) ] ]; body [] [node] ]

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
    article [cls "panel is-primary"]
      [ p [cls "panel-heading"] [text "Todo List"]
      ; div [cls "panel-block"] [add_item]
      ; div [] (model.todos |> List.map item) ]
    |> scaffold
end
