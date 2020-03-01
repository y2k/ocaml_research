let (>>) f g x = g (f x)

module Domain = struct
  let isUrl text = 
    let r = Str.regexp "https://" in
    Str.string_match r text 0
end

module Component = struct
  type model = { user : string; count : int }
  let init_model = { user = ""; count = 0 }
  type msg =
    | Init 
    | NewLine of string 
    | UrlLoaded of string
  type cmd = 
    | LoadUrl of string * (string -> msg) 
    | Repl of string * (string -> msg)

  let update model msg = 
    match msg with
    | Init -> model, [ Repl ("Enter url to load", fun line -> NewLine line) ]
    | NewLine line ->
      if "" = line 
        then model, [ Repl ("Enter url to load", fun line -> NewLine line) ]
      else if Domain.isUrl line 
        then model, [ LoadUrl (line, fun url -> UrlLoaded url) ]
      else 
        model, [ Repl (Printf.sprintf "Invalid URL %s" line, fun line -> NewLine line) ]
    | UrlLoaded html -> 
      { model with count = model.count + 1 }, 
      [ Repl (Printf.sprintf "Url loaded = %d" (String.length html), fun line -> NewLine line) ]
end

module Effetcs = struct
  open Lwt
  open Cohttp_lwt_unix
  
  let load_url url =
    Uri.of_string url
    |> Client.get
    >>= fun (_, body) -> Cohttp_lwt.Body.to_string body

  let repl text =
    Printf.printf "%s\n>>> " text;
    read_line () |> Lwt.return
end

open Lwt

let () = 
  let cur_model = ref Component.init_model in
  let rec loop msg : unit Lwt.t =
    let (model2, cmds) = Component.update !cur_model msg in
    cur_model := model2;
    cmds
    |> List.map (fun cmd -> 
      match cmd with
      | Component.LoadUrl (url, next) -> Effetcs.load_url url >>= (next >> loop)
      | Component.Repl (text, next) -> Effetcs.repl text >>= (next >> loop))
    |> Lwt.all
    |> Lwt.map ignore in
  Lwt_main.run (loop Component.Init)
