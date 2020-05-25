open Prelude
module M = Navigation

module Application = struct
  open Diff
  open Printf

  let prev_state : node list ref = ref []

  let dispatch (msg : M.Update.msg) : string =
    M.Update.Serializer.serialize msg |> Remote.Dispatch.json_to_websocket_msg

  let render_dynamic model =
    let v = M.View.view model dispatch in
    let prev = !prev_state in
    prev_state := [v] ;
    Diff.compute_diff prev [v] []

  let render_string model =
    let id_to_string complex_id =
      complex_id |> List.map string_of_int
      |> Listy.reduce (sprintf "%s-%s") (fun _ -> "")
    in
    render_dynamic model
    |> List.map (function
         | AddNode (parent_id, id, name) ->
             `Assoc
               [ ("t", `String "a")
               ; ("p", `String (id_to_string parent_id))
               ; ("i", `String (id_to_string id))
               ; ("n", `String name) ]
         | RemoveNode id ->
             `Assoc [("t", `String "r"); ("i", `String (id_to_string id))]
         | SetProp (id, key, value) ->
             `Assoc
               [ ("t", `String "s")
               ; ("i", `String (id_to_string id))
               ; ("n", `String key)
               ; ("v", `String value) ]
         | RemoveProp (id, key) ->
             `Assoc
               [ ("t", `String "d")
               ; ("i", `String (id_to_string id))
               ; ("n", `String key) ])
    |> fun x -> Yojson.to_string (`List x)
end

let hot_reload_file = ref ""

module Websocket_client = struct
  open Lwt.Infix
  open Websocket
  open Websocket_lwt_unix

  let section = Lwt_log.Section.make "websocket"

  let try_load_state_from_disk () : M.Update.model option =
    match !hot_reload_file with
    | file when file <> "" && Sys.file_exists file ->
        let channel = open_in_bin file in
        let m : M.Update.model = Marshal.from_channel channel in
        close_in channel ; Some m
    | _ ->
        None

  let try_save_hot_reload (model : M.Update.model) =
    match !hot_reload_file with
    | "" ->
        ()
    | file ->
        let channel = open_out_bin file in
        Marshal.to_channel channel model [] ;
        close_out channel

  let model = ref @@ fst M.Update.init

  let try_reload_model_from_disk () =
    try_load_state_from_disk () |> Option.iter @@ fun x -> model := x

  let render_view () = Application.render_string !model

  let handle_message dispatch msg =
    let new_model, effects = M.Update.update dispatch !model msg in
    model := new_model ;
    try_save_hot_reload new_model ;
    Remote.EffectHandler.run_effects effects ;
    render_view ()

  let update_and_render (send_ui : string -> unit) (msg_text : string) =
    let rec dispatch (msg : M.Update.msg) =
      handle_message dispatch msg |> send_ui
    in
    let msg : M.Update.msg =
      Remote.Dispatch.parse msg_text M.Update.Serializer.deserialize
    in
    handle_message dispatch msg

  let handler id client =
    incr id ;
    let id = !id in
    let send = Connected_client.send client in
    Lwt_log.ign_info_f ~section "New connection (id = %d)" id ;
    Lwt.async (fun () ->
        Application.prev_state := [] ;
        send @@ Frame.create ~content:(render_view ()) ()) ;
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
            let send_ui result =
              Lwt.async (fun _ -> send @@ Frame.create ~content:result ())
            in
            let result = update_and_render send_ui fr.content in
            send @@ Frame.create ~content:result ()
        | Opcode.Binary ->
            send @@ Frame.create ~content:"[]" ()
        | _ ->
            send @@ Frame.close 1002 >>= fun () -> Lwt.fail Exit
      in
      Connected_client.recv client >>= react >>= recv_forever
    in
    Lwt.catch recv_forever (fun exn ->
        Lwt_log.info_f ~section "Connection to client %d lost (%s)" id
          (Printexc.to_string exn)
        >>= fun () -> Lwt.return_unit)

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
module C = Cohttp.Cookie.Cookie_hdr
module H = Cohttp.Header

let render _ = Diff.Renderer.render |> Dsl.render

let add_sign_cookies (req : Request.t) =
  let cookies = C.extract req.headers in
  if List.exists (fun (k, _) -> k = "user_id") cookies then H.init ()
  else (
    Random.self_init () ;
    let id =
      List.init 5 (fun _ -> Random.int 1000000)
      |> List.fold_left (Printf.sprintf "%s-%i") ""
    in
    let k, v = C.serialize [("user_id", id)] in
    H.init_with k v )

let server_callback _ (req : Request.t) body =
  print_endline @@ "resource = " ^ req.resource ;
  match req.resource with
  | "/" ->
      let cookies = add_sign_cookies req in
      Body.to_string body
      >>= fun form ->
      Server.respond_string ~headers:cookies ~status:`OK ~body:(render form) ()
  | "/sw.js" | "/favicon.ico" ->
      Server.respond_not_found ()
  | path ->
      Server.respond_file ~fname:("output" ^ path) ()

let parse_args () =
  let speclist =
    [("-hr", Arg.String (fun d -> hot_reload_file := d), "Hot-Reload file")]
  in
  Arg.parse speclist print_endline ""

let run () =
  parse_args () ;
  Websocket_client.try_reload_model_from_disk () ;
  [ Server.create
      ~mode:(`TCP (`Port 8080))
      (Server.make ~callback:server_callback ())
  ; Websocket_client.start ]
  |> Lwt.all |> Lwt.map ignore |> Lwt_main.run
