open Lib.Feed_screen

let test_post : Lib.Feed_domain.post =
  { title= ""
  ; image=
      [|{url= "http://img1.joyreactor.cc/pics/post/-5940877.jpeg"; aspect= 1.5}|]
  }

module TestRunner = struct
  module E = Lib.Remote.EffectHandler

  let run_init init update view =
    let msg_buffer = ref [] in
    let model, effs = init (fun m -> msg_buffer := m :: !msg_buffer) in
    view ignore model |> ignore ;
    effs |> List.iter (fun eff -> E.run_effect_lwt eff |> Lwt_main.run) ;
    !msg_buffer
    |> List.fold_left (fun model msg -> update ignore model msg |> fst) model

  let run_update update view model msg =
    view ignore model |> ignore ;
    let msg_buffer = ref [] in
    let model, effs =
      update (fun m -> msg_buffer := m :: !msg_buffer) model msg
    in
    view ignore model |> ignore ;
    effs |> List.iter (fun eff -> E.run_effect_lwt eff |> Lwt_main.run) ;
    !msg_buffer
    |> List.fold_left (fun model msg -> update ignore model msg |> fst) model
end

let%test _ =
  let model, _ = init ignore in
  model = {posts= []; status= Loading}

let%test "first open screen" =
  let model = TestRunner.run_init init update view in
  model.status = Finished && List.length model.posts = 0

let%test "second open screen with cached data" =
  TestRunner.run_init init update view |> ignore ;
  let model = TestRunner.run_init init update view in
  model.status = Finished && List.length model.posts > 0

let%test _ =
  let model = {posts= []; status= Loading} in
  let model =
    TestRunner.run_update update view model (PostsLoaded (Ok [test_post]))
  in
  model.status = Finished && List.length model.posts > 0
