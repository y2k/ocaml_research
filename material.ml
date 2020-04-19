module Update = struct
  type model = unit

  type msg = ServerUpdate of string

  let init = ((), [])

  let update model _msg = (model, [])
end

module View = struct
  open Dsl

  let viewButton title =
    button [cls "mdc-button"]
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

  let viewContent =
    div
      [("style", "display: flex; flex-direction: column")]
      [ textField "todo text"
      ; viewButton "create"
      ; viewCart "Item #1"
      ; viewCart "Item #2" ]

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
          [ viewContent
          ; script []
              [ text
                  "mdc.ripple.MDCRipple.attachTo(document.querySelector('.mdc-button')); \
                   mdc.textField.MDCTextField.attachTo(document.querySelector('.mdc-text-field'));"
              ] ] ]
end