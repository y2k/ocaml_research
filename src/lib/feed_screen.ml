open Prelude
open Feed_domain
module D = Storage.TodoStoreReducer

type model = {posts: post list; status: status}

type msg = PostsLoaded of (post list, exn) result | StoreLoaded of D.store

let postsLoaded x = PostsLoaded x

let init dispatch =
  ( {posts= []; status= Loading}
  , [ `SendEvent (D.TodoInvalidated, fun db -> StoreLoaded db |> dispatch)
    ; `WebRequest
        ( "https://jrs.y2k.work/parse/http%3A%2F%2Fjoyreactor.cc%2Ftag%2F%D0%BC%D0%B5%D0%BC%D1%8B"
        , Result.map parse_posts >> postsLoaded >> dispatch ) ] )

let update dispatch model = function
  | StoreLoaded db ->
      ( { model with
          posts= db.feed |> List.filter (fun x -> Array.length x.image > 0) }
      , [] )
  | PostsLoaded (Ok posts) ->
      ( {model with status= Finished}
      , [`SendEvent (D.UpdateFeed posts, fun db -> StoreLoaded db |> dispatch)]
      )
  | PostsLoaded (Error _) ->
      ({model with status= Error}, [])

open Dsl
module M = Dsl.Material

module Image = struct
  let normalize url (w : float) (h : float) =
    Printf.sprintf
      "https://rc.y2k.work/cache/fit?width=%i&height=%i&bgColor=ffffff&quality=75&url=%s"
      (int_of_float w) (int_of_float h) (Uri.pct_encode url)

  let urlWithHeight limitWidth (attachment : attachment) =
    let aspect = max 1.2 attachment.aspect in
    let w = limitWidth in
    let h = w /. aspect in
    let out_aspect = 100. /. aspect in
    (normalize attachment.url w h, out_aspect)
end

let view_item post =
  div
    [("style", "padding: 0px 4px 4px")]
    [ div
        [ ( "style"
          , let url, aspect = Image.urlWithHeight 300. post.image.(0) in
            Printf.sprintf
              "width: 100%%; padding-top: %f%%; background-size: 100%% 100%%; \
               background-image: url(%s)"
              aspect url ) ]
        [] ]

let view_progress = function
  | Loading ->
      M.linear_progress [("indeterminate", "")]
  | _ ->
      div [] []

let view _dispatch model =
  div []
    [view_progress model.status; M.list [] (model.posts |> List.map view_item)]
