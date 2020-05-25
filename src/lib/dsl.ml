type lazyNode = {token: bytes; view: node Lazy.t}

and node =
  { tag: string
  ; props: (string * string) list
  ; children: node list
  ; lazyNode: lazyNode option }

let cls name = ("class", name)

let a props children = {tag= "a"; props; children; lazyNode= None}

let article props children = {tag= "article"; props; children; lazyNode= None}

let img props = {tag= "img"; props; children= []; lazyNode= None}

let body props children = {tag= "body"; props; children; lazyNode= None}

let button props children = {tag= "button"; props; children; lazyNode= None}

let div props children = {tag= "div"; props; children; lazyNode= None}

let footer props children = {tag= "footer"; props; children; lazyNode= None}

let form props children = {tag= "form"; props; children; lazyNode= None}

let label props children = {tag= "label"; props; children; lazyNode= None}

let h1 props children = {tag= "h1"; props; children; lazyNode= None}

let h2 props children = {tag= "h2"; props; children; lazyNode= None}

let h3 props children = {tag= "h3"; props; children; lazyNode= None}

let h4 props children = {tag= "h4"; props; children; lazyNode= None}

let h5 props children = {tag= "h5"; props; children; lazyNode= None}

let h6 props children = {tag= "h6"; props; children; lazyNode= None}

let head props children = {tag= "head"; props; children; lazyNode= None}

let script props children = {tag= "script"; props; children; lazyNode= None}

let html props children = {tag= "html"; props; children; lazyNode= None}

let input props children = {tag= "input"; props; children; lazyNode= None}

let link props = {tag= "link"; props; children= []; lazyNode= None}

let meta props = {tag= "meta"; props; children= []; lazyNode= None}

let nav props children = {tag= "nav"; props; children; lazyNode= None}

let p props children = {tag= "p"; props; children; lazyNode= None}

let progress props children = {tag= "progress"; props; children; lazyNode= None}

let section props children = {tag= "section"; props; children; lazyNode= None}

let span props children = {tag= "span"; props; children; lazyNode= None}

let text value = {tag= ""; props= [("", value)]; children= []; lazyNode= None}

let title props children = {tag= "title"; props; children; lazyNode= None}

let value_source = "__VALUE__"

let rec render node =
  match node.tag with
  | "" ->
      List.hd node.props |> snd
  | _ ->
      let ps =
        List.fold_left
          (fun a (k, v) -> Printf.sprintf "%s %s='%s'" a k v)
          "" node.props
      in
      let chs = List.fold_left (fun a n -> a ^ render n) "" node.children in
      Printf.sprintf "<%s%s>%s</%s>" node.tag ps chs node.tag

module Material = struct
  let snackbar props children =
    {tag= "mwc-snackbar"; props; children; lazyNode= None}

  let linear_progress props =
    {tag= "mwc-linear-progress"; props; children= []; lazyNode= None}

  let textfield props =
    {tag= "mwc-textfield"; props; children= []; lazyNode= None}

  let list_item props children =
    {tag= "mwc-list-item"; props; children; lazyNode= None}

  let list props children = {tag= "mwc-list"; props; children; lazyNode= None}

  let button props = {tag= "mwc-button"; props; children= []; lazyNode= None}

  let icon_button props =
    {tag= "mwc-icon-button"; props; children= []; lazyNode= None}

  let top_app_bar props children =
    {tag= "mwc-top-app-bar"; props; children; lazyNode= None}
end
