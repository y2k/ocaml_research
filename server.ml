open Cohttp_lwt_unix

let run =
  let callback _conn _req _body =
     Server.respond_string ~status:`OK ~body:(Dsl.page ()) () in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())
