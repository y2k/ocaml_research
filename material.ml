module Update = struct
  type model = int

  type msg = ServerUpdate of string

  let init = (0, [])

  let update model msg =
    match msg with ServerUpdate "create" -> (model + 1, []) | _ -> (model, [])
end

module View = struct
  open Dsl

  let viewButton title =
    button
      [cls "mdc-button"; ("onclick", "remote_ui_ws.send('create')")]
      [ div [cls "mdc-button__ripple"] []
      ; span [cls "mdc-button__label"] [text title] ]

  let textField hint =
    div [cls "mdc-text-field"]
      [ input [cls "mdc-text-field__input"] []
      ; div [cls "mdc-line-ripple"] []
      ; label
          [cls "mdc-floating-label"; ("for", "text-field-hero-input")]
          [text hint] ]

  let viewCart title =
    let actionButton title =
      button
        [cls "mdc-button mdc-card__action mdc-card__action--button"]
        [ div [cls "mdc-button__ripple"] []
        ; span [cls "mdc-button__label"] [text title] ]
    in
    div
      [cls "mdc-card"; ("style", "margin: 8px")]
      [ div
          [ cls "mdc-card__primary-action"
          ; ("tabindex", "0")
          ; ("style", "padding: 8px") ]
          [div [] [h3 [cls "mdc-typography mdc-typography--body2"] [text title]]]
      ; div [cls "mdc-card__action-icons"] [actionButton "delete"] ]

  let viewContent (model : Update.model) =
    div
      [("style", "display: flex; flex-direction: column")]
      [ textField "todo text"
      ; viewButton @@ Printf.sprintf "create (%i)" model
      ; (if model <> 1 then viewCart "Item #1" else div [] [])
      ; viewCart ("Item #2 - " ^ string_of_int model) ]

  let view (_model : Update.model) =
    html []
      [ head []
          [ link
              [ ( "href"
                , "https://unpkg.com/material-components-web@v4.0.0/dist/material-components-web.min.css"
                )
              ; ("rel", "stylesheet") ]
          ; link
              [ ("rel", "stylesheet")
              ; ( "href"
                , "https://fonts.googleapis.com/icon?family=Material+Icons" ) ]
          ; script
              [ ( "src"
                , "https://unpkg.com/material-components-web@v4.0.0/dist/material-components-web.min.js"
                ) ]
              [] ]
      ; body []
          [ viewContent 0
          ; script []
              [ text
                  "mdc.ripple.MDCRipple.attachTo(document.querySelector('.mdc-button')); \
                   mdc.textField.MDCTextField.attachTo(document.querySelector('.mdc-text-field'));"
              ] ] ]
end
