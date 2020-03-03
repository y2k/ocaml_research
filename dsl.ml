type node = {tag: string; props: (string * string) list; children: node list}

let cls name = ("class", name)
let html props children = {tag= "html"; props; children}
let body props children = {tag= "body"; props; children}
let head props children = {tag= "head"; props; children}
let article props children = {tag= "article"; props; children}
let title props children = {tag= "title"; props; children}
let form props children = {tag= "form"; props; children}
let footer props children = {tag= "footer"; props; children}
let div props children = {tag= "div"; props; children}
let input props children = {tag= "input"; props; children}
let a props children = {tag= "a"; props; children}
let p props children = {tag= "p"; props; children}
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
