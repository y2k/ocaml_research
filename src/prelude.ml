let ( >> ) f g x = g (f x)

let hd_opt = function x :: _ -> Some x | [] -> None
