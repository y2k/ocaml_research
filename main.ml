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
