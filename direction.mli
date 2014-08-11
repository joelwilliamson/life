(** A type to recognize the four off-axis directions on the grid *)
type t =
	| NW
	| NE
	| SE
	| SW

(** Convert a direction to a pair of booleans, the first encoding whether this
  * is a northerly direction, the second whether it is easterly *)
val to_ne : t -> (bool * bool)

(** The inverse function of to_ne *)
val from_ne : (bool * bool) -> t
