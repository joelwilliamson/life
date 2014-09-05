(** An axis-aligned bounding box
  * This is a pair of half-open intervals, both centered on a given point,
  * but with possibly different lengths *)
type t = {
	center : Point.t ;
	half_dimension : Point.t
	}

(** Construct an AABB *)
val create : Point.t -> Point.t -> t

(** Check if the given point is in the AABB *)
val contains : t -> Point.t -> bool

(** Check if a point is in the interior of the AABB. This is equivalent to
 ** checking if all its neighbours are contained by the BB *)

(** Check if the two boxes intersect *)
val intersect : t -> t -> bool

(** Give the direction from the center of the box to the point *)
val dir_to_point : t -> Point.t -> Direction.t

(** Return a box twice as large in each axis, with the center shifted in the
  * given direction *)
val quadruple : t -> Direction.t -> t

(** Return a tuple of the quarters of this box, ordered NW,NE,SE,SW *)
val quadrants : t -> (t * t * t * t)

val to_string : t -> string
