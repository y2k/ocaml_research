open Lib

let () =
  let db = Storage.TodoStore.open_db "main.db" in
  (Remote.EffectHandler.store := fun _ -> db) ;
  Lib.Main.run ()
