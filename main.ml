open Lwt
open Cohttp_lwt
open Cohttp_lwt_unix

let shared_state = ref (fst Todolist.Update.init)

let render form =
  let (new_state, _) = Todolist.Update.update !shared_state (Todolist.Update.ServerUpdate form) in
  shared_state := new_state;
  Todolist.View.view new_state
  |> Dsl.render

let () =
  let callback _conn _req body =
    Body.to_string body
    >>= fun form -> Server.respond_string ~status:`OK ~body:(render form) () in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())
  |> Lwt_main.run
