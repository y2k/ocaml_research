open Prelude

type model =
  {city: string; temp: float option; error: string option; loading: bool}

type msg =
  | LoadTemperature
  | CityChanged of string
  | LoadTemperatureEnd of (float, exn) result

let loadTemperatureEnd x = LoadTemperatureEnd x

let deserialize =
  let module D = Decoders_yojson.Basic.Decode in
  D.at ["main"; "temp"] D.float

let init = ({city= ""; temp= None; error= None; loading= false}, [])

let update (dispatch : msg -> unit) model msg =
  match msg with
  | CityChanged city ->
      ({model with city}, [])
  | LoadTemperature ->
      let token = Sys.getenv "OPEN_WEATHER_API" in
      ( {model with error= None; loading= true}
      , [ download
            (Printf.sprintf
               "https://api.openweathermap.org/data/2.5/weather?appid=%s&q=%s&units=metric&lang=en"
               token model.city)
            deserialize
            (loadTemperatureEnd >> dispatch) ] )
  | LoadTemperatureEnd (Ok temp) ->
      ({model with temp= Some temp; loading= false}, [])
  | LoadTemperatureEnd (Error error) ->
      ({model with error= Some (Printexc.to_string error); loading= false}, [])

open Dsl
module M = Dsl.Material

let view_temp model =
  match model.temp with
  | Some temp ->
      h1 [("style", "align-self: center")] [text @@ Printf.sprintf "%g C" temp]
  | None ->
      div [] []

let view_progress = function
  | true ->
      M.linear_progress [("indeterminate", "")]
  | false ->
      div [] []

let view_error model =
  match model.error with
  | None ->
      div [] []
  | Some error ->
      M.snackbar [("isopen", ""); ("labelText", error)] []

let view dispatch model =
  div
    [("style", "display: flex; flex-direction: column")]
    [ M.textfield
        [ ("label", "Enter city name")
        ; ("value", model.city)
        ; ("oninput", dispatch @@ CityChanged value_source) ]
    ; view_progress model.loading
    ; M.button
        [ ("label", "Load wheather")
        ; ("style", "margin: 4px")
        ; ("raised", "")
        ; ("onclick", dispatch LoadTemperature) ]
    ; view_temp model
    ; view_error model ]
