open Prelude
module D = Storage.TodoStoreReducer

type model =
  { city: string
  ; temp: float option
  ; error: string option
  ; loading: bool
  ; favorite: string list }

type msg =
  | AddToFavorite
  | DeleteFavorite of int
  | SelectFavorite of int
  | LoadTemperature
  | CityChanged of string
  | LoadTemperatureEnd of (float, exn) result
  | StoreUpdated of D.store

let loadTemperatureEnd x = LoadTemperatureEnd x

let deserialize =
  let module D = Decoders_yojson.Basic.Decode in
  D.at ["main"; "temp"] D.float

let init dispatch =
  ( {city= ""; temp= None; error= None; loading= false; favorite= []}
  , [`SendEvent (D.TodoInvalidated, fun db -> StoreUpdated db |> dispatch)] )

let downloadWeather dispatch token city =
  download
    (Printf.sprintf
       "https://api.openweathermap.org/data/2.5/weather?appid=%s&q=%s&units=metric&lang=en"
       token city)
    deserialize
    (loadTemperatureEnd >> dispatch)

let update (dispatch : msg -> unit) model msg =
  match msg with
  | StoreUpdated db ->
      ({model with favorite= db.favorite_cities}, [])
  | AddToFavorite ->
      ( model
      , [ `SendEvent
            (D.CityFavorited model.city, fun db -> StoreUpdated db |> dispatch)
        ] )
  | DeleteFavorite i ->
      ( model
      , [ `SendEvent
            ( D.CityUnfavorited (List.nth model.favorite i)
            , fun db -> StoreUpdated db |> dispatch ) ] )
  | SelectFavorite i ->
      ({model with city= List.nth model.favorite i}, [])
  | CityChanged city ->
      ({model with city}, [])
  | LoadTemperature ->
      let token = Sys.getenv "OPEN_WEATHER_API" in
      ( {model with error= None; loading= true}
      , [downloadWeather dispatch token model.city] )
  | LoadTemperatureEnd (Ok temp) ->
      ({model with temp= Some temp; loading= false}, [])
  | LoadTemperatureEnd (Error error) ->
      ({model with error= Some (Printexc.to_string error); loading= false}, [])

module View = struct
  open Dsl
  module M = Dsl.Material

  let viewItem dispatch i x =
    M.list_item
      [("hasmeta", ""); ("onclick", dispatch @@ SelectFavorite i)]
      [ span [] [text x]
      ; span
          [ ("slot", "meta")
          ; cls "material-icons"
          ; ("onclick", dispatch @@ DeleteFavorite i) ]
          [text "delete"] ]

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
      [ div
          [("style", "display: flex; align-items: center")]
          [ M.textfield
              [ ("style", "flex: 1")
              ; ("label", "Enter city name")
              ; ("value", model.city)
              ; ("oninput", dispatch @@ CityChanged value_source) ]
          ; M.button
              [ ("label", "★")
              ; ("raised", "")
              ; ("onclick", dispatch AddToFavorite) ] ]
      ; view_progress model.loading
      ; M.button
          [ ("label", "Load wheather")
          ; ("style", "margin: 4px")
          ; ("raised", "")
          ; ("onclick", dispatch LoadTemperature) ]
      ; view_temp model
      ; M.list [] (model.favorite |> List.mapi (viewItem dispatch))
      ; view_error model ]
end
