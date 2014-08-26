type t =
        | White
	| Black
	| Red
	| Green
	| Blue

val int_of_color : t -> int

val of_string : string -> t
