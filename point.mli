type t = int * int

(* Construct a point from a pair *)
val create : (int * int) -> t

(* Get the x component of a point *)
val x : t -> int

(* Get the y component of a point *)
val y : t -> int

(* Add two points *)
val add : t -> t -> t

(* Subtract two points *)
val sub : t -> t -> t

(* Add the x component, subtract the y *)
val addsub : t -> t -> t

(* Subtract the x component, add the y *)
val subadd : t -> t -> t

(** Convert the point to a string *)
val to_string : t -> string
