type t =
	| White
	| Black
	| Red
	| Green
	| Blue

let int_of_color = function
	| White -> 0xFFFFFF
	| Black -> 0x000000
	| Red ->   0xFF0000
	| Green -> 0x00FF00
	| Blue ->  0x0000FF

let (>>) f g x = f x |> g
let of_string =
	String.lowercase >> (function
                | "white" -> White
                | "black" -> Black
                | "red" -> Red
                | "blue" -> Blue
                | "green" -> Green
                | _ -> failwith ("Invalid color")) 
