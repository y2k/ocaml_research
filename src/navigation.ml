module Update = struct
  type sub_model =
    | Main of Todolist_screen.Update.model
    | Examples of Examples_screen.Update.model
    | Weather of Weather_screen.model
    | FeedModel of Feed_screen.model

  type model = {current: sub_model; history: sub_model list}

  type msg =
    | MainMsg of Todolist_screen.Update.msg
    | ExamplesMsg of Examples_screen.Update.msg
    | WeatherMsg of Weather_screen.msg
    | NavigateBack
    | FeedMsg of Feed_screen.msg

  module Serializer = struct
    module E = Examples_screen.Update

    let serialize = function
      | NavigateBack ->
          `String "nb"
      | MainMsg smsg ->
          `Assoc [("m", Todolist_screen.Update.Serializer.serialize smsg)]
      | WeatherMsg smsg ->
          `Assoc
            [ ( "wm"
              , match smsg with
                | LoadTemperature ->
                    `Assoc [("lt", `Null)]
                | CityChanged x ->
                    `Assoc [("ch", `String x)]
                | _ ->
                    failwith "unsupported variant" ) ]
      | ExamplesMsg smsg ->
          `Assoc
            [ ( "m2"
              , match smsg with
                | E.OpenFeed ->
                    `String "of"
                | E.OpenWeather ->
                    `String "ow"
                | E.OpenTodoList ->
                    `String "otl" ) ]
      | _ ->
          failwith ""

    let deserialize json =
      match json with
      | `String "nb" ->
          NavigateBack
      | `Assoc [("m", sjson)] ->
          MainMsg (Todolist_screen.Update.Serializer.deserialize sjson)
      | `Assoc [("wm", sjson)] ->
          WeatherMsg
            ( match sjson with
            | `Assoc [("lt", `Null)] ->
                LoadTemperature
            | `Assoc [("ch", `String x)] ->
                CityChanged x
            | _ ->
                failwith @@ "illegal json = " ^ Yojson.Basic.show json )
      | `Assoc [("m2", sjson)] -> (
        match sjson with
        | `String "of" ->
            ExamplesMsg E.OpenFeed
        | `String "ow" ->
            ExamplesMsg E.OpenWeather
        | `String "otl" ->
            ExamplesMsg E.OpenTodoList
        | _ ->
            failwith @@ "illegal json = " ^ Yojson.Basic.show json )
      | _ ->
          failwith @@ "illegal json = " ^ Yojson.Basic.show json
  end

  let init =
    let sm, _ = Examples_screen.Update.init in
    ({current= Examples sm; history= []}, [])

  let update dispatch model msg =
    match (model.current, msg) with
    | _, NavigateBack -> (
      match model.history with
      | current :: history ->
          ({current; history}, [])
      | _ ->
          (model, []) )
    | _, ExamplesMsg Examples_screen.Update.OpenTodoList ->
        let sm, effs = Todolist_screen.Update.init in
        ({current= Main sm; history= model.current :: model.history}, effs)
    | _, ExamplesMsg Examples_screen.Update.OpenWeather ->
        let sm, effs = Weather_screen.init in
        ({current= Weather sm; history= model.current :: model.history}, effs)
    | _, ExamplesMsg Examples_screen.Update.OpenFeed ->
        let sm, effs = Feed_screen.init (fun x -> FeedMsg x |> dispatch) in
        ({current= FeedModel sm; history= model.current :: model.history}, effs)
    | Main sm, MainMsg smsg ->
        let sm, effs = Todolist_screen.Update.update sm smsg in
        ({model with current= Main sm}, effs)
    | Weather sm, WeatherMsg smsg ->
        let sm, effs =
          Weather_screen.update (fun x -> WeatherMsg x |> dispatch) sm smsg
        in
        ({model with current= Weather sm}, effs)
    | FeedModel sm, FeedMsg smsg ->
        let sm, effs =
          Feed_screen.update (fun x -> FeedMsg x |> dispatch) sm smsg
        in
        ({model with current= FeedModel sm}, effs)
    | _ ->
        failwith "unhandled state"
end

module View = struct
  open Dsl
  open Update
  module M = Dsl.Material

  let view_content model dispatch =
    match model with
    | Main sub_model ->
        Diff.LazyView.view sub_model
          (Todolist_screen.View.view (fun x -> MainMsg x |> dispatch))
    | Examples sub_model ->
        Diff.LazyView.view sub_model
          (Examples_screen.View.view (fun x -> ExamplesMsg x |> dispatch))
    | Weather sub_model ->
        Diff.LazyView.view sub_model
          (Weather_screen.view (fun x -> WeatherMsg x |> dispatch))
    | FeedModel sub_model ->
        Diff.LazyView.view sub_model
          (Feed_screen.view (fun x -> FeedMsg x |> dispatch))

  let view_back_button dispatch history =
    match history with
    | [] ->
        div [] []
    | _ :: _ ->
        M.icon_button
          [ ("icon", "arrow_back")
          ; ("slot", "navigationIcon")
          ; ("onclick", dispatch NavigateBack) ]

  let view (model : Update.model) dispatch =
    M.top_app_bar []
      [ view_back_button dispatch model.history
      ; div [("slot", "title")] [text "OCaml remote research"]
      ; view_content model.current dispatch ]
end
