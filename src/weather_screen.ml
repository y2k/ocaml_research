open Prelude

type model = {city: string; temp: float option; error: bool}

type msg =
  | LoadTemperature
  | CityChanged of string
  | LoadTemperatureEnd of (float, exn) result

let loadTemperatureEnd x = LoadTemperatureEnd x

let deserialize json =
  let open Yojson.Basic.Util in
  json |> member "main" |> member "temp" |> to_number_option

let download (url : string) deserialize (dispatch : _ -> unit) =
  let decode f json_text =
    let json = Yojson.Basic.from_string json_text in
    f json |> Option.to_result ~none:(Invalid_argument (Yojson.Basic.show json))
  in
  let f x = x |> Fun.flip Result.bind (decode deserialize) |> dispatch in
  `WebRequest (url, f)

let init = ({city= ""; temp= None; error= false}, [])

let update (dispatch : msg -> unit) model msg =
  match msg with
  | CityChanged city ->
      ({model with city}, [])
  | LoadTemperature ->
      let token = Sys.getenv "OPEN_WEATHER_API" in
      ( {model with error= false}
      , [ download
            (Printf.sprintf
               "https://api.openweathermap.org/data/2.5/weather?appid=%s&q=%s&units=metric&lang=en"
               token model.city)
            deserialize
            (loadTemperatureEnd >> dispatch) ] )
  | LoadTemperatureEnd (Ok temp) ->
      ({model with temp= Some temp}, [])
  | LoadTemperatureEnd (Error _) ->
      ({model with error= true}, [])

open Dsl
module M = Dsl.Material

let view_temp model =
  match model.temp with
  | Some temp ->
      h1 [("style", "align-self: center")] [text @@ Printf.sprintf "%g C" temp]
  | None ->
      div [] []

let view dispatch model =
  div
    [("style", "display: flex; flex-direction: column")]
    [ M.textfield
        [ ("label", "Enter city name")
        ; ("value", model.city)
        ; ("oninput", dispatch @@ CityChanged value_source) ]
    ; M.button
        [ ("label", "Load wheather")
        ; ("style", "margin: 4px")
        ; ("raised", "")
        ; ("onclick", dispatch LoadTemperature) ]
    ; view_temp model
    ; M.snackbar
        [ ((if model.error then "isopen" else "_isopen"), "")
        ; ("labelText", "Error, try again later") ]
        [] ]
