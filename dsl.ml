type node = {tag: string; props: (string * string) list; children: node list}

let html props children = {tag= "html"; props; children}
let body props children = {tag= "body"; props; children}
let head props children = {tag= "head"; props; children}
let title props children = {tag= "title"; props; children}
let div props children = {tag= "div"; props; children}
let span props children = {tag= "span"; props; children}
let link props = {tag= "link"; props; children= []}
let progress props children = {tag= "progress"; props; children}
let meta props = {tag= "meta"; props; children= []}
let button props children = {tag= "button"; props; children}
let text value = {tag= ""; props= [("", value)]; children= []}

let rec render node =
  match node.tag with
  | "" -> List.hd node.props |> snd
  | _ ->
      let ps =
        List.fold_left
          (fun a (k, v) -> Printf.sprintf "%s %s='%s'" a k v)
          "" node.props in
      let chs = List.fold_left (fun a n -> a ^ render n) "" node.children in
      Printf.sprintf "<%s%s>%s</%s>" node.tag ps chs node.tag

let page () =
  html []
    [ head []
        [ title [] [text "OCaml Research"]; meta [("charset", "utf-8")]
        ; meta
            [ ("name", "viewport")
            ; ("content", "width=device-width, initial-scale=1") ]
        ; link
            [ ("rel", "stylesheet")
            ; ( "href"
              , "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css" )
            ] ]
    ; body []
        [ div [("class", "tags")]
            [ span [("class", "tag")] [text "юмор"]
            ; span [("class", "tag")] [text "games"]
            ; span [("class", "tag")] [text "cats"] ]
        ; progress [("class", "progress is-info"); ("max", "100")] [text "30%"]
        ; div
            [("class", "notification is-link")]
            [ button [("class", "delete")] []
            ; text
                {|Lorem ipsum dolor sit amet, consectetur adipiscing elit loremipsum dolor.|}
            ]; button [("class", "button is-danger")] [text "Danger"] ] ]
  |> render
