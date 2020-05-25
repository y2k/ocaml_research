open Prelude

type attachment = {url: string; aspect: float}
[@@deriving yojson {strict= false}]

type post = {title: string; image: attachment array}
[@@deriving yojson {strict= false}]

let parse_posts json =
  let open Yojson.Safe.Util in
  json |> Yojson.Safe.from_string |> member "posts" |> member "posts" |> to_list
  |> List.map (fun yojson ->
         post_of_yojson yojson
         |> function
         | Ok x ->
             x
         | Error e ->
             fail @@ __LOC_OF__ ("((" ^ e ^ ")) " ^ Yojson.Safe.to_string yojson))
