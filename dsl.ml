type node = {tag: string; props: (string * string) list; children: node list}

let cls name = ("class", name)
let a props children = {tag= "a"; props; children}
let article props children = {tag= "article"; props; children}
let body props children = {tag= "body"; props; children}
let button props children = {tag= "button"; props; children}
let div props children = {tag= "div"; props; children}
let footer props children = {tag= "footer"; props; children}
let form props children = {tag= "form"; props; children}
let h1 props children = {tag= "h1"; props; children}
let h2 props children = {tag= "h2"; props; children}
let h3 props children = {tag= "h3"; props; children}
let h4 props children = {tag= "h4"; props; children}
let h5 props children = {tag= "h5"; props; children}
let h6 props children = {tag= "h6"; props; children}
let head props children = {tag= "head"; props; children}
let html props children = {tag= "html"; props; children}
let input props children = {tag= "input"; props; children}
let link props = {tag= "link"; props; children= []}
let meta props = {tag= "meta"; props; children= []}
let nav props children = {tag= "nav"; props; children}
let p props children = {tag= "p"; props; children}
let progress props children = {tag= "progress"; props; children}
let section props children = {tag= "section"; props; children}
let span props children = {tag= "span"; props; children}
let text value = {tag= ""; props= [("", value)]; children= []}
let title props children = {tag= "title"; props; children}

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

let scaffold node =
  html []
    [ head []
        [ title [] [text "OCaml Research"]; meta [("charset", "utf-8")]
        ; meta
            [ ("name", "viewport")
            ; ( "content"
              , {|width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no|}
              ) ]
        ; link
            [ ("rel", "stylesheet")
            ; ( "href"
              , "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css" )
            ] ]; body [] [node] ]
