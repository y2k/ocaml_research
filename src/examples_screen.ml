module Update = struct
  type msg = OpenWeather | OpenTodoList

  type item = {text: string; msg: msg}

  type model = {items: item list}

  let init =
    ( { items=
          [ {text= "Todo List example"; msg= OpenTodoList}
          ; {text= "Weather example"; msg= OpenWeather} ] }
    , [] )

  let update model _ = (model, [])
end

module View = struct
  open Dsl
  open Update
  module M = Dsl.Material

  let view_item dispatch (item : item) =
    M.list_item [("onclick", dispatch item.msg)] [text item.text]

  let view dispatch (model : model) =
    M.list [] (model.items |> List.map (view_item dispatch))
end
