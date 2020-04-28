let wrap_update update smodel smsg dispatch fmsg fmodel =
  let m, e = update smodel smsg (fun x -> dispatch @@ fmsg x) in
  (fmodel m, e)

let wrap_init update dispatch fmsg fmodel =
  let m, e = update (fun x -> dispatch @@ fmsg x) in
  (fmodel m, e)

module ChildComponent = struct
  type model = Model

  type msg = ReadIntEnd of int | ReadStringEnd of string

  let init dispatch =
    ( Model
    , [ `ReadIntEffect ("pref1", fun x -> dispatch @@ ReadIntEnd x)
      ; `ReadStringEffect ("pref2", fun x -> dispatch @@ ReadStringEnd x) ] )
end

let test () =
  let m : ChildComponent.msg option ref = ref None in
  let dispatch (_msg : ChildComponent.msg) : unit = m := Some _msg in
  let _, a = ChildComponent.init dispatch in
  match a with
  | [`ReadIntEffect (_param, _f)] ->
      _f 0 ;
      let _a = Option.get !m in
      ()
  | _ ->
      ()

(* module ParentComponent = struct
  type model = Model1 of ChildComponent.model | Model2 of ChildComponent.model

  type msg = Msg1 of ChildComponent.msg | Msg2 of ChildComponent.msg

  let int dispatch =
    wrap_init ChildComponent.init dispatch (fun x -> Msg1 x) (fun x -> Model1 x)
end *)
