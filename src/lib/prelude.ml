let ( >> ) f g x = g (f x)

module Listy = struct
  let reduce f empty xs =
    match xs with
    | [x] ->
        x
    | x :: xs ->
        xs |> List.fold_left f x
    | [] ->
        empty ()
end

let fail (line, msg) = failwith (line ^ " - " ^ msg)

let hd_opt = function x :: _ -> Some x | [] -> None

type status = Loading | Error | Finished

let download (url : string) decoder (dispatch : _ -> unit) =
  let module D = Decoders_yojson.Basic.Decode in
  let decode (f : _ -> _ result) json_text =
    Yojson.Basic.from_string json_text
    |> f
    |> Result.map_error D.string_of_error
    |> Result.map_error (fun e -> Invalid_argument e)
  in
  let f x =
    x |> Fun.flip Result.bind (decode (D.decode_value decoder)) |> dispatch
  in
  `WebRequest (url, f)
