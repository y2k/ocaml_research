type attachment = {url: string; aspect: float} [@@deriving yojson]

type post = {title: string; image: attachment option} [@@deriving yojson]

let parse_posts json =
  let open Yojson.Safe.Util in
  json |> Yojson.Safe.from_string |> member "posts" |> member "posts" |> to_list
  |> List.map @@ fun json -> post_of_yojson json |> Result.get_ok
