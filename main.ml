module Websocket_client = struct
  open Lwt.Infix
  open Websocket
  open Websocket_lwt_unix

  let section = Lwt_log.Section.make "reynir"

  let handler id client =
    incr id ;
    let id = !id in
    let send = Connected_client.send client in
    Lwt_log.ign_info_f ~section "New connection (id = %d)" id ;
    Lwt.async (fun () ->
        Lwt_unix.sleep 1.0
        >>= fun () -> send @@ Frame.create ~content:"Delayed message" ()) ;
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
        | Opcode.Text | Opcode.Binary ->
            send @@ Frame.create ~content:"OK" ()
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

(* module Screen = Todolist *)
module Screen = Material

let shared_state = ref (fst Screen.Update.init)

let render form =
  let new_state, _ =
    Screen.Update.update !shared_state (Screen.Update.ServerUpdate form)
  in
  shared_state := new_state ;
  Screen.View.view new_state |> Dsl.render

let () =
  let callback _conn _req body =
    Body.to_string body
    >>= fun form -> Server.respond_string ~status:`OK ~body:(render form) ()
  in
  Server.create ~mode:(`TCP (`Port 8080)) (Server.make ~callback ())
  |> Lwt_main.run
