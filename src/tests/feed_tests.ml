open Lib.Feed_screen
module E = Lib.Remote.EffectHandler

let%test _ =
  let model, _ = init ignore in
  model = {posts= []; status= Loading}

let%test _ =
  let msg_buffer = ref [] in
  let model, effs = init (fun m -> msg_buffer := m :: !msg_buffer) in
  view ignore model |> ignore ;
  effs |> List.iter (fun eff -> E.run_effect_lwt eff |> Lwt_main.run) ;
  let model =
    !msg_buffer
    |> List.fold_left (fun model msg -> update ignore model msg |> fst) model
  in
  view ignore model |> ignore ;
  model.status = Finished && List.length model.posts = 0

let%test _ =
  let model = {posts= []; status= Loading} in
  let msg_buffer = ref [] in
  let model, effs =
    update
      (fun m -> msg_buffer := m :: !msg_buffer)
      model
      (PostsLoaded
         (Ok
            [ { title= ""
              ; image=
                  [| { url= "http://img1.joyreactor.cc/pics/post/-5940877.jpeg"
                     ; aspect= 1.5 } |] } ]))
  in
  effs |> List.iter (fun eff -> E.run_effect_lwt eff |> Lwt_main.run) ;
  let model =
    !msg_buffer
    |> List.fold_left (fun model msg -> update ignore model msg |> fst) model
  in
  model.status = Finished && List.length model.posts > 0
