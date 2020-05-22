open Prelude

type attachment = {url: string; aspect: float}

type post = {title: string; image: attachment option}

let parse_posts json =
  let open Yojson.Basic.Util in
  json |> Yojson.Basic.from_string |> member "posts" |> member "posts"
  |> to_list
  |> List.map
     @@ fun json ->
     { title= json |> member "title" |> to_string
     ; image=
         ( json |> member "image" |> to_list |> hd_opt
         |> Option.map
            @@ fun json ->
            { url= json |> member "url" |> to_string
            ; aspect= json |> member "aspect" |> to_float } ) }
