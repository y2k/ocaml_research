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

type status = Loading | Error | Finished

type model = {posts: post list; status: status}

type msg = PostsLoaded of (post list, exn) result

let postsLoaded x = PostsLoaded x

let init dispatch =
  ( {posts= []; status= Loading}
  , [ `WebRequest
        ( "https://jrs.y2k.work/parse/http%3A%2F%2Fjoyreactor.cc%2Ftag%2F%D0%BC%D0%B5%D0%BC%D1%8B"
        , Result.map parse_posts >> postsLoaded >> dispatch ) ] )

let update _dispatch model = function
  | PostsLoaded (Ok posts) ->
      ( { posts= posts |> List.filter (fun x -> Option.is_some x.image)
        ; status= Finished }
      , [] )
  | PostsLoaded (Error _) ->
      ({model with status= Error}, [])

open Dsl
module M = Dsl.Material

let view_item post =
  div
    [("style", "padding: 0px 4px 4px")]
    [ img
        [ ("style", "width: 100%")
        ; ("src", (Option.get post.image).url)
        ; ("loading", "lazy") ] ]

let view _dispatch model = M.list [] (model.posts |> List.map view_item)
