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

  let invoke_constructor (type' : string) (args : remote_type list) :
      remote_type Lwt.t =
    let request =
      [ ("action", ["constructor"])
      ; ("type", [type'])
      ; ("args", args |> List.map type_to_string) ]
    in
    let%lwt response = Client.send request in
    Lwt.return @@ string_to_type response
end

module RemoteTransaction = struct
  type env = {context: Remote.context; activity: Remote.context}

  let run (f : env -> unit Lwt.t) : unit Lwt.t =
    Remote.Client.begin_scope () ;%lwt
    let e = {context= {obj= {handler= 0}}; activity= {obj= {handler= 0}}} in
    f e ;%lwt Remote.Client.end_scope ()
end

class toast id =
  let open Lwt in
  object
    method show = Remote.invoke_method {handler= id} "show" [] >|= ignore

    method setGravity g x y : unit Lwt.t =
      Remote.invoke_method {handler= id} "setGravity" [RInt g; RInt x; RInt y]
      >|= ignore
  end

module Toast = struct
  open Remote

  let _LENGTH_SHORT = 0

  let _LENGTH_LONG = 1

  type t = {id: Remote.obj}

  let makeText (ctx : Remote.context) (text : string) (duration : int) =
    [RObj ctx.obj; RString text; RInt duration]
    |> Remote.invoke_static_method "android.widget.Toast" "makeText"
    |> Lwt.map (function
         | Remote.RObj obj ->
             new toast obj.handler
         | _ ->
             failwith "illegal type")
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

class notification (self : int) =
  object
    method self = self
  end

class notification_manager (self : int) =
  let open Remote in
  let open Lwt in
  object
    method notify (id : int) (n : notification) : unit Lwt.t =
      Remote.invoke_method {handler= self} "notify"
        [RInt id; RObj {handler= n#self}]
      >|= ignore
  end

module NotificationManager = struct
  open Remote

  let from (ctx : Remote.context) =
    Remote.invoke_static_method "androidx.core.app.NotificationManagerCompat"
      "from" [RObj ctx.obj]
    |> Lwt.map (function
         | RObj self ->
             new notification_manager self.handler
         | _ ->
             failwith "???")
end

class notification_builder (handler : int) =
  let open Remote in
  object
    method setContentTitle (title : string) =
      invoke_method {handler} "setContentTitle" [RString title]
      |> Lwt.map (function
           | RObj obj ->
               new notification_builder obj.handler
           | _ ->
               failwith "???")

    method setContentText (text : string) =
      invoke_method {handler} "setContentText" [RString text]
      |> Lwt.map (function
           | RObj obj ->
               new notification_builder obj.handler
           | _ ->
               failwith "???")

    method setSmallIcon (drawable : int) =
      invoke_method {handler} "setSmallIcon" [RInt drawable]
      |> Lwt.map (function
           | RObj obj ->
               new notification_builder obj.handler
           | _ ->
               failwith "???")

    method build =
      invoke_method {handler} "build" []
      |> Lwt.map (function
           | RObj obj ->
               new notification obj.handler
           | _ ->
               failwith "???")
  end

module NotificationBuilder = struct
  open Remote

  let build (ctx : context) (channel_id : string) : notification_builder Lwt.t =
    invoke_constructor "androidx.core.app.NotificationCompat$Builder"
      [RObj ctx.obj; RString channel_id]
    |> Lwt.map (function
         | RObj obj ->
             new notification_builder obj.handler
         | _ ->
             failwith "???")
end

module OcamlUtils = struct
  open Remote

  let drawable (name : string) : int Lwt.t =
    invoke_static_method "io.y2k.android_remote.OcamlUtils" "drawable"
      [RString name]
    |> Lwt.map @@ function RInt x -> x | _ -> failwith "???"
end

module Example = struct
  open Lwt

  let ( !! ) x = Option.get x

  let _REQUEST_IMAGE_CAPTURE = 2000

  let setImageBitmap (_ : Bitmap.t) : unit = ()

  let show_notificatio (env : RemoteTransaction.env) =
    let%lwt icon = OcamlUtils.drawable "ic_notification" in
    let%lwt nb = NotificationBuilder.build env.context "default" in
    let%lwt _ = nb#setContentTitle "Hello" in
    let%lwt _ = nb#setContentText "from OCaml" in
    let%lwt _ = nb#setSmallIcon icon in
    let%lwt n = nb#build in
    let%lwt nm = NotificationManager.from env.context in
    nm#notify 1 n

  let show_toast (env : RemoteTransaction.env) =
    let%lwt toast = Toast.makeText env.context "Hello" Toast._LENGTH_SHORT in
    toast#show ;%lwt
    let%lwt toast =
      Toast.makeText env.context "from OCaml" Toast._LENGTH_LONG
    in
    toast#setGravity Gravity._CENTER 0 0 ;%lwt
    toast#show

  let request_image_from_camera (env : RemoteTransaction.env) =
    let%lwt intent = Intent.new' Intent._IMAGE_CAPTURE in
    let%lwt exists = Intent.resolveActivity intent env.context in
    if Option.is_some exists then
      Activity.startActivityForResult (Activity.fromEnv env) intent
        _REQUEST_IMAGE_CAPTURE
    else return ()

  let receive_image_from_camera (env : RemoteTransaction.env) =
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

  let main () = RemoteTransaction.run show_notificatio
end
