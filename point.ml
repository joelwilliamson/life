open Core.Std

type t = (int * int)
let create (x,y) = (x,y)

let x (x,_) = x

let y (_,y) = y

let add (x1,y1) (x2,y2) = (x1+x2),(y1+y2)

let sub (x1,y1) (x2,y2) = (x1-x2),(y1-y2)

let addsub (x1,y1) (x2,y2) = (x1+x2),(y1-y2)

let subadd (x1,y1) (x2,y2) = (x1-x2),(y1+y2)

let to_string (x,y) = (string_of_int x)^","^(string_of_int y)

let of_string s = String.split ~on:',' s
	|> List.map ~f:int_of_string
	|> function
		| x::y::[] -> (x,y)
		| _ -> failwith ("Invalid arg to Point.of_string: " ^ s)
