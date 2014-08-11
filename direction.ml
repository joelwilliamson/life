type t =
	| NW | NE | SE | SW ;;

let to_ne = function
	| NW -> true,false
	| NE -> true,true
	| SE -> false,true
	| SW -> false,false

let from_ne = function
	| true,false -> NW
	| true,true -> NE
	| false,true -> SE
	| false,false -> SW
