type model = {city: string; temp: float}

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

let ( >> ) f g x = g (f x)

let update (dispatch : msg -> unit) model msg =
  match msg with
  | CityChanged city ->
      ({model with city}, [])
  | LoadTemperature ->
      ( model
      , [ download
            "https://api.openweathermap.org/data/2.5/weather?q=$city&units=metric&lang=en"
            deserialize
            (loadTemperatureEnd >> dispatch) ] )
  | LoadTemperatureEnd (Ok temp) ->
      ({model with temp}, [])
  | LoadTemperatureEnd (Error _) ->
      failwith "???"

open Dsl
module M = Dsl.Material

let view model dispatch =
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
        ; ("onclick", dispatch LoadTemperature) ] ]
