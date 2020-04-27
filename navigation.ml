module Update = struct
  type sub_model = Main of Main_screen.Update.model

  type model = {current: sub_model; history: sub_model list}

  type msg = MainMsg of Main_screen.Update.msg

  module Serializer = struct
    let serialize = function
      | MainMsg smsg ->
          `Assoc [("m", Main_screen.Update.Serializer.serialize smsg)]

    let deserialize json =
      match json with
      | `Assoc [("m", sjson)] ->
          MainMsg (Main_screen.Update.Serializer.deserialize sjson)
      | _ ->
          failwith "???"
  end

  let init =
    let sm, _ = Main_screen.Update.init in
    ({current= Main sm; history= []}, [])

  let update model msg =
    match (model.current, msg) with
    | Main sm, MainMsg smsg ->
        let sm, effs = Main_screen.Update.update sm smsg in
        ({model with current= Main sm}, effs)
end

module View = struct
  open Dsl
  open Update
  module M = Dsl.Material

  let view_context model dispatch =
    match model with
    | Main sm ->
        Diff.LazyView.view sm (fun sm -> Main_screen.View.view sm dispatch)

  (* Main_screen.View.view sm dispatch *)

  let view (model : Update.model) dispatch =
    M.top_app_bar []
      [ M.icon_button [("icon", "menu"); ("slot", "navigationIcon")]
      ; div [("slot", "title")] [text "OCaml remote research"]
      ; view_context model.current (fun x -> MainMsg x |> dispatch) ]
end
