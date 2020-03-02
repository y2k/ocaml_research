module HtmlDsl = struct
  type node = {tag: string; props: (string * string) list; children: node list}

  let html props children = {tag= "html"; props; children}
  let body props children = {tag= "body"; props; children}
  let head props children = {tag= "head"; props; children}
  let title props children = {tag= "title"; props; children}
  let div props children = {tag= "div"; props; children}
  let text value = {tag= ""; props= [("", value)]; children= []}

  let rec render node =
    match node.tag with
    | "" -> List.hd node.props |> snd
    | _ ->
        let ps =
          List.fold_left
            (fun a (k, v) -> Printf.sprintf "%s %s=%s" a k v)
            "" node.props in
        let chs = List.fold_left (fun a n -> a ^ render n) "" node.children in
        Printf.sprintf "<%s %s>%s</%s>" node.tag ps chs node.tag

  let test () =
    html []
      [ head [] [text "Cool site :)"]
      ; body []
          [ div [] []
          ; div []
              [ html []
                  [head [] [text "Cool site :)"]; body [] [div [] []; div [] []]]
              ] ] ]
end
