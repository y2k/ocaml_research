let ( >> ) f g x = g (f x)

module Domain = struct
  let isUrl text =
    let r = Str.regexp {|\(https://\|http://\).+|} in
    Str.string_match r text 0

  let getTitle html =
    let r = Str.regexp {|.+<title>\(.+?\)</title>.+|} in
    if Str.string_match r html 0 then Some (Str.matched_group 1 html) else None
end

module Component = struct
  type model = {titles: string list; count: int}

  let init_model = {titles= []; count= 0}

  type msg = Init | NewLine of string | StringDownloaded of string

  type cmd =
    | DowloadString of string * (string -> msg)
    | Repl of string * (string -> msg)

  let update model msg =
    match msg with
    | Init -> (model, [Repl ("Enter url to load", fun line -> NewLine line)])
    | NewLine line ->
        if "" = line then
          (model, [Repl ("Enter url to load", fun line -> NewLine line)])
        else if Domain.isUrl line then
          (model, [DowloadString (line, fun url -> StringDownloaded url)])
        else
          ( model
          , [ Repl
                (Printf.sprintf "Invalid URL %s" line, fun line -> NewLine line)
            ] )
    | StringDownloaded html ->
        ( {model with count= model.count + 1}
        , [ Repl
              ( Printf.sprintf "Url loaded (%d) = %s" model.count
                  (Domain.getTitle html |> Option.value ~default:"<no title>")
              , fun line -> NewLine line ) ] )
end

module Effetcs = struct
  open Lwt
  open Cohttp_lwt_unix

  let load_url url =
    Uri.of_string url |> Client.get
    >>= fun (_, body) -> Cohttp_lwt.Body.to_string body

  let repl text =
    Lwt_io.printf "%s\n>>> " text >>= fun _ -> Lwt_io.read_line Lwt_io.stdin
end

(* open Lwt *)

let () =
  (* let cur_model = ref Component.init_model in
  let rec loop msg : unit Lwt.t =
    let model2, cmds = Component.update !cur_model msg in
    cur_model := model2 ;
    cmds
    |> List.map (fun cmd ->
           match cmd with
           | Component.DowloadString (url, next) ->
               Effetcs.load_url url >>= (next >> loop)
           | Component.Repl (text, next) -> Effetcs.repl text >>= (next >> loop))
    |> Lwt.all |> Lwt.map ignore in *)
  [(* loop Component.Init; *) Server.run] |> Lwt.all |> Lwt_main.run |> ignore
