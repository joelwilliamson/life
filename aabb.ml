open Core.Std

type t = {
	center : Point.t ;
	half_dimension : Point.t ;
	}

let create center half_dimension =
	let rec is_power_2 = function
		| 0 -> true
		| 1 -> true
		| n -> (n%2 = 0) && is_power_2 (n/2) in
	if (Point.x half_dimension < 0) || (Point.y half_dimension < 0)
	then failwith "Negative dimension not permitted."
	else if not ((Point.x half_dimension |> is_power_2) && (Point.y half_dimension |> is_power_2))
	then failwith "Dimensions must be power of two"
	else {center; half_dimension}

let contains aabb (x,y) =
	let contains_x  = if (Point.x aabb.half_dimension) = 0
		then (Point.x aabb.center) = x
		else (Point.x aabb.center - Point.x aabb.half_dimension) <= x
			&& x < (Point.x aabb.center + Point.x aabb.half_dimension)
	and contains_y = if (Point.y aabb.half_dimension) = 0
		then (Point.y aabb.center) = y
		else (Point.y aabb.center - Point.y aabb.half_dimension) <= y
			&& y < (Point.y aabb.center + Point.y aabb.half_dimension)
	in contains_x && contains_y

let is_interior aabb p =
	let contains_x  = if (Point.x aabb.half_dimension) = 0
		then false
		else (Point.x aabb.center - Point.x aabb.half_dimension) <= (x-1)
			&& (x+1) < (Point.x aabb.center + Point.x aabb.half_dimension)
	and contains_y = if (Point.y aabb.half_dimension) = 0
		then false
		else (Point.y aabb.center - Point.y aabb.half_dimension) <= (y-1)
			&& (y+1) < (Point.y aabb.center + Point.y aabb.half_dimension)
	in contains_x && contains_y
	
	
let intersect a b =
	let x_d = (Point.x a.center) - (Point.x b.center) |> abs
	and y_d = (Point.y a.center) - (Point.y b.center) |> abs
	and max_x = (Point.x a.half_dimension) + (Point.x b.half_dimension)
	and max_y = (Point.y a.half_dimension) + (Point.y b.half_dimension)
	in (x_d <= max_x) && (y_d <= max_y)
	
let dir_to_point aabb (x,y) =
	let north = y >= (Point.y aabb.center)
	and east  = x >= (Point.x aabb.center)
	in Direction.from_ne (north,east)

let quadruple orig d =
	let half_dimension = (2 * (Point.x orig.half_dimension)), (2 * (Point.y orig.half_dimension))
	and north,east = Direction.to_ne d in
	let c_x = if east
		then (Point.x orig.center) + (Point.x orig.half_dimension)
		else (Point.x orig.center) - (Point.x orig.half_dimension)
	and c_y = if north
		then (Point.y orig.center) + (Point.y orig.half_dimension)
		else (Point.y orig.center) - (Point.y orig.half_dimension)
	in let center = (c_x,c_y)
	in {half_dimension; center}

let quadrants bb =
	let half_round_up n = (n/2) + (n%2) in
	let half_dimension = half_round_up (Point.x bb.half_dimension),
		half_round_up (Point.y bb.half_dimension) in
	let nw = (Point.x bb.center) - (Point.x half_dimension), (Point.y bb.center) + (Point.y half_dimension)
	and ne = (Point.x bb.center) + (Point.x half_dimension), (Point.y bb.center) + (Point.y half_dimension)
	and se = (Point.x bb.center) + (Point.x half_dimension), (Point.y bb.center) - (Point.y half_dimension)
	and sw = (Point.x bb.center) - (Point.x half_dimension), (Point.y bb.center) - (Point.y half_dimension)
	in ({center=nw;half_dimension},{center=ne;half_dimension},
	{center=se;half_dimension},{center=sw;half_dimension})

let to_string x =
	"{center = "^(Point.to_string x.center)^
	";half_dimension = "^(Point.to_string x.half_dimension)
