(*

Toast.makeText(context, "hello", Toast.LENGTH_LONG).show()
Toast.makeText(App.instance, "hello", 0).show()
android.widget.Toast.makeText(App.instance, "hello", 0).show()
android.widget.Toast.makeText(<context>, "hello", 0).show()

(.show
  (android.widget.Toast.makeText
    (android-context)
    "hello"
    0))
*)

module Remote = struct
  type obj = {handler: int}

  type context = {obj: obj}

  type remote_type = RInt of int | RString of string | RObj of obj | RVoid

  module Client = struct
    open Lwt
    open Cohttp_lwt
    open Cohttp_lwt_unix

    let send (form : (string * string list) list) : string Lwt.t =
      let%lwt _, body =
        Uri.of_string "http://192.168.1.33:10000/"
        |> Client.post ~chunked:false
             ~body:(Body.of_string (Uri.encoded_of_query form))
      in
      Body.to_string body

    let begin_scope () : unit Lwt.t =
      send [("action", ["begin-scope"])] >|= ignore

    let end_scope () : unit Lwt.t = send [("action", ["end-scope"])] >|= ignore
  end

  let type_to_string = function
    | RInt x ->
        "i-" ^ string_of_int x
    | RString x ->
        "s-" ^ x
    | RObj x ->
        "o-" ^ string_of_int x.handler
    | RVoid ->
        "v-"

  let string_to_type x =
    let value = lazy (String.sub x 2 (String.length x - 2)) in
    match String.sub x 0 2 with
    | "i-" ->
        RInt (Lazy.force value |> int_of_string)
    | "s-" ->
        RString (Lazy.force value)
    | "o-" ->
        RObj {handler= Lazy.force value |> int_of_string}
    | "v-" ->
        RVoid
    | _ ->
        failwith @@ Printf.sprintf "Usuported type = %s" x

  let invoke_static_method (type' : string) (method' : string)
      (args : remote_type list) : remote_type Lwt.t =
    let request =
      [ ("action", ["static-method"])
      ; ("type", [type'])
      ; ("method", [method'])
      ; ("args", args |> List.map type_to_string) ]
    in
    let%lwt response = Client.send request in
    Lwt.return @@ string_to_type response

  let invoke_method (instance : obj) (method' : string)
      (args : remote_type list) : remote_type Lwt.t =
    let request =
      [ ("action", ["method"])
      ; ("instance", [string_of_int instance.handler])
      ; ("method", [method'])
      ; ("args", args |> List.map type_to_string) ]
    in
    let%lwt response = Client.send request in
    Lwt.return @@ string_to_type response

  let invoke_constructor (_type : string) (_args : remote_type list) : obj Lwt.t
      =
    failwith "???"
end

module RemoteTransaction = struct
  type env = {context: Remote.context; activity: Remote.context}

  let run (f : env -> unit Lwt.t) : unit Lwt.t =
    Remote.Client.begin_scope () ;%lwt
    let e = {context= {obj= {handler= 0}}; activity= {obj= {handler= 0}}} in
    f e ;%lwt Remote.Client.end_scope ()
end

module Toast = struct
  open Remote
  open Lwt

  let _LENGTH_SHORT = 0

  let _LENGTH_LONG = 1

  type t = {toast: Remote.obj}

  let makeText (ctx : Remote.context) (text : string) (duration : int) : t Lwt.t
      =
    [RObj ctx.obj; RString text; RInt duration]
    |> Remote.invoke_static_method "android.widget.Toast" "makeText"
    |> Lwt.map (function
         | Remote.RObj toast ->
             {toast}
         | _ ->
             failwith "illegal type")

  let show (this : t) : unit Lwt.t =
    Remote.invoke_method this.toast "show" [] >|= ignore

  let setGravity (this : t) g x y : unit Lwt.t =
    Remote.invoke_method this.toast "setGravity" [RInt g; RInt x; RInt y]
    >|= ignore
end

module Bundle = struct
  type t = {obj: Remote.obj}

  let get (this : t) (key : string) : Remote.obj Lwt.t =
    Remote.invoke_method this.obj "get" [Remote.RString key]
    |> Lwt.map (function Remote.RObj x -> x | _ -> failwith "illegal type")
end

module Intent = struct
  let _IMAGE_CAPTURE = "android.media.action.IMAGE_CAPTURE"

  type t = {obj: Remote.obj}

  let new' (_ : string) : t Lwt.t = failwith "???"

  let extras (_ : t) : Bundle.t option Lwt.t = failwith "???"

  let resolveActivity (_ : t) (_ : Remote.context) : Remote.obj option Lwt.t =
    failwith "???"
end

module Activity = struct
  type t = {obj: Remote.obj}

  let _RESULT_OK = -1

  let fromEnv (_ : RemoteTransaction.env) : t = failwith "???"

  let startActivityForResult (_ : t) (_ : Intent.t) (_ : int) : unit Lwt.t =
    failwith "???"
end

module Environment = struct
  let onActivityResult _ : (int * int * Intent.t option) Lwt.t = failwith "???"
end

module Bitmap = struct
  open Remote

  type t = {obj: Remote.obj}

  let width (this : t) : int Lwt.t =
    Remote.invoke_method this.obj "getWidth" []
    |> Lwt.map (function RInt x -> x | _ -> failwith "illegal result type")

  let height (_ : t) : int Lwt.t = failwith "???"

  let cast (obj : Remote.obj) : t = {obj}
end

module Gravity = struct
  let _CENTER = 17
end

module Example = struct
  open Lwt

  let ( !! ) x = Option.get x

  let _REQUEST_IMAGE_CAPTURE = 2000

  let setImageBitmap (_ : Bitmap.t) : unit = ()

  let show_toast () =
    RemoteTransaction.run
    @@ fun env ->
    let%lwt toast = Toast.makeText env.context "hello" Toast._LENGTH_SHORT in
    Toast.show toast ;%lwt
    let%lwt toast =
      Toast.makeText env.context "from ocaml" Toast._LENGTH_LONG
    in
    Toast.setGravity toast Gravity._CENTER 0 0 ;%lwt
    Toast.show toast

  let request_image_from_camera () =
    RemoteTransaction.run
    @@ fun env ->
    let%lwt intent = Intent.new' Intent._IMAGE_CAPTURE in
    let%lwt exists = Intent.resolveActivity intent env.context in
    if Option.is_some exists then
      Activity.startActivityForResult (Activity.fromEnv env) intent
        _REQUEST_IMAGE_CAPTURE
    else return ()

  let receive_image_from_camera () =
    RemoteTransaction.run
    @@ fun env ->
    let%lwt requestCode, resultCode, data = Environment.onActivityResult env in
    if requestCode = _REQUEST_IMAGE_CAPTURE && resultCode = Activity._RESULT_OK
    then (
      let%lwt extras = Intent.extras !!data in
      let%lwt image_bitmap = Bundle.get !!extras "data" >|= Bitmap.cast in
      let%lwt width = Bitmap.width image_bitmap in
      let%lwt height = Bitmap.height image_bitmap in
      Printf.printf "size = %i x %i" width height |> ignore ;
      return @@ setImageBitmap image_bitmap )
    else return ()
end
