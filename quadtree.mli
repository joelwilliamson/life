(** Quadtree - A Quadtree is a two-dimensional tree used to store many points
  * on a grid. Each node is either a bounded box containing a single point, or
  * four quadrants, each containing a quadtree
  * See https://en.wikipedia.org/wiki/Quadtree for more details
  *)
type data =
	| Point of Point.t
	| Empty
	| Node of t * t * t * t (* NW, NE, SE, SW *)
and t = Aabb.t * data

(** Construct an empty tree with the given bounding box *)
val empty : Aabb.t -> t

(** Construct a tree containing the given point in the bounding box *)
val singleton : Aabb.t -> Point.t -> t

(** Add the point to the tree *)
val insert : t -> p:Point.t -> t

(** Check if the point has been added to the tree *)
val contains : t -> Point.t -> bool

(** Return a list of all the points in the tree *)
val to_list : t -> Point.t list

(** Convert the tree to a string. The string is a sorted list of the points in
  * the tree. This is a normalized representation, so it is suitable for
  * checking two trees for equality
  **)
val to_string : t -> string

val of_string : string -> t
