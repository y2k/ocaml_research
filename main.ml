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
  open Dsl
  open Diff
  open Printf

  let prev_state : node list ref = ref []

  let render_dynamic model =
    let v = Material.View.viewContent model in
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
             sprintf
               {|{ "tag" : "add-node", "parent_id": "%s", "id" : "%s", "name" : "%s"}|}
               (id_to_string parent_id) (id_to_string id) name
         | RemoveNode id ->
             sprintf {|{ "tag" : "remove-node", "id" : "%s" }|}
               (id_to_string id)
         | SetProp (id, key, value) ->
             sprintf
               {|{ "tag" : "set-prop", "id" : "%s", "name" : "%s", "value" : "%s"}|}
               (id_to_string id) key value
         | RemoveProp (id, key) ->
             sprintf {|{ "tag" : "remove-prop", "id" : "%s", "name" : "%s"}|}
               (id_to_string id) key)
    |> ListEx.reduce (Printf.sprintf "%s, %s") (fun _ -> "")
    |> sprintf "[ %s ]"

  let render_static =
    html []
      [ head []
          [ link
              [ ("rel", "stylesheet")
              ; ( "href"
                , "https://unpkg.com/material-components-web@v4.0.0/dist/material-components-web.min.css"
                ) ]
          ; link
              [ ("rel", "stylesheet")
              ; ( "href"
                , "https://fonts.googleapis.com/icon?family=Material+Icons" ) ]
          ; script
              [ ( "src"
                , "https://unpkg.com/material-components-web@v4.0.0/dist/material-components-web.min.js"
                ) ]
              [] ]
      ; body []
          [ div [("id", "container")] []
          ; script []
              [ text
                  {|
                    (function() {
                      remote_ui_ws = new WebSocket('ws://localhost:8081/');
                      remote_ui_ws.onmessage = function(msg) {
                        const cmds = JSON.parse(msg.data);
                        for (cmd of cmds) {
                          switch (cmd.tag) {
                            case "add-node":
                              if (cmd.name != "") {
                                const node = document.createElement(cmd.name);
                                node.id = cmd.id;
                                document.getElementById(cmd.parent_id || "container").appendChild(node);
                              }
                              break;
                            case "remove-node":
                              document.getElementById(cmd.id).remove();
                              break;
                            case "set-prop":
                              if (cmd.name == "") {
                                document.getElementById(cmd.id.substring(2) || "container").innerText = cmd.value;
                              } else {
                                document.getElementById(cmd.id).setAttribute(cmd.name, cmd.value)
                              }
                              break;
                            case "remove-prop":
                              document.getElementById(cmd.id).removeAttribute(cmd.name)
                              break;
                          }
                        }
                      };
                    })();
                  |}
              ] ] ]
end

module Websocket_client = struct
  open Lwt.Infix
  open Websocket
  open Websocket_lwt_unix

  let section = Lwt_log.Section.make "websocket"

  let model = ref @@ fst Material.Update.init

  let render_view () = Application.render_string !model

  let update_and_render (msg : string) =
    model :=
      fst @@ Material.Update.update !model (Material.Update.ServerUpdate msg) ;
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
            send @@ Frame.create ~content:(Application.render_string 0) ()
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

let render _form = Application.render_static |> Dsl.render

let () =
  let callback _conn _req body =
    Body.to_string body
    >>= fun form -> Server.respond_string ~status:`OK ~body:(render form) ()
  in
  [ Server.create ~mode:(`TCP (`Port 8080)) (Server.make ~callback ())
  ; Websocket_client.start ]
  |> Lwt.all |> Lwt.map ignore |> Lwt_main.run
