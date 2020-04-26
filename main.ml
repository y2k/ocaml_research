module ListEx = struct
  let reduce f empty xs =
    match xs with
    | [x] ->
        x
    | x :: xs ->
        xs |> List.fold_left f x
    | [] ->
        empty ()
end

module Application = struct
  open Diff
  open Printf

  let prev_state : node list ref = ref []

  let render_dynamic model =
    let v = Material.View.view model in
    let prev = !prev_state in
    prev_state := [v] ;
    Diff.compute_diff prev [v] []

  let render_string model =
    let id_to_string complex_id =
      complex_id |> List.map string_of_int
      |> ListEx.reduce (sprintf "%s-%s") (fun _ -> "")
    in
    render_dynamic model
    |> List.map (function
         | AddNode (parent_id, id, name) ->
             `Assoc
               [ ("tag", `String "add-node")
               ; ("parent_id", `String (id_to_string parent_id))
               ; ("id", `String (id_to_string id))
               ; ("name", `String name) ]
         | RemoveNode id ->
             `Assoc
               [ ("tag", `String "remove-node")
               ; ("id", `String (id_to_string id)) ]
         | SetProp (id, key, value) ->
             `Assoc
               [ ("tag", `String "set-prop")
               ; ("id", `String (id_to_string id))
               ; ("name", `String key)
               ; ("value", `String value) ]
         | RemoveProp (id, key) ->
             `Assoc
               [ ("tag", `String "remove-prop")
               ; ("id", `String (id_to_string id))
               ; ("name", `String key) ])
    |> fun x -> Yojson.to_string (`List x)
end

module Websocket_client = struct
  open Lwt.Infix
  open Websocket
  open Websocket_lwt_unix

  let section = Lwt_log.Section.make "websocket"

  let model = ref @@ fst Material.Update.init

  let render_view () = Application.render_string !model

  let update_and_render (msg_text : string) =
    let msg : Material.Update.msg =
      Remote.Dispatch.parse msg_text Material.Update.Serializer.deserialize
    in
    let new_model, effects = Material.Update.update !model msg in
    model := new_model ;
    Remote.EffectHandler.run_effects effects ;
    render_view ()

  let handler id client =
    incr id ;
    let id = !id in
    let send = Connected_client.send client in
    Lwt_log.ign_info_f ~section "New connection (id = %d)" id ;
    Lwt.async (fun () -> send @@ Frame.create ~content:(render_view ()) ()) ;
    let rec recv_forever () =
      let open Frame in
      let react fr =
        Lwt_log.debug_f ~section "<- %s" (Frame.show fr)
        >>= fun () ->
        match fr.opcode with
        | Opcode.Ping ->
            send @@ Frame.create ~opcode:Opcode.Pong ~content:fr.content ()
        | Opcode.Close ->
            Lwt_log.info_f ~section "Client %d sent a close frame" id
            >>= fun () ->
            (* Immediately echo and pass this last message to the user *)
            ( if String.length fr.content >= 2 then
              send
              @@ Frame.create ~opcode:Opcode.Close
                   ~content:(String.sub fr.content 0 2)
                   ()
            else send @@ Frame.close 1000 )
            >>= fun () -> Lwt.fail Exit
        | Opcode.Pong ->
            Lwt.return_unit
        | Opcode.Text ->
            let result = update_and_render fr.content in
            send @@ Frame.create ~content:result ()
        | Opcode.Binary ->
            send @@ Frame.create ~content:"[]" ()
        | _ ->
            send @@ Frame.close 1002 >>= fun () -> Lwt.fail Exit
      in
      Connected_client.recv client >>= react >>= recv_forever
    in
    Lwt.catch recv_forever (fun exn ->
        Lwt_log.info_f ~section "Connection to client %d lost" id
        >>= fun () -> Lwt.fail exn)

  let start =
    let uri = Uri.of_string "http://localhost:8081" in
    Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system
    >>= fun endp ->
    let open Conduit_lwt_unix in
    endp_to_server ~ctx:default_ctx endp
    >>= fun server ->
    establish_server ~ctx:default_ctx ~mode:server (handler @@ ref (-1))
end

open Lwt
open Cohttp_lwt
open Cohttp_lwt_unix

let render _ = Diff.Renderer.render |> Dsl.render

let server_callback _ (req : Request.t) body =
  print_endline @@ "resource = " ^ req.resource ;
  match req.resource with
  | "/" ->
      Body.to_string body
      >>= fun form -> Server.respond_string ~status:`OK ~body:(render form) ()
  | "/sw.js" | "/favicon.ico" ->
      Server.respond_not_found ()
  | path ->
      Server.respond_file ~fname:("output" ^ path) ()

let () =
  [ Server.create
      ~mode:(`TCP (`Port 8080))
      (Server.make ~callback:server_callback ())
  ; Websocket_client.start ]
  |> Lwt.all |> Lwt.map ignore |> Lwt_main.run
