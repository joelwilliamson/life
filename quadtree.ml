open Core.Std ;;

type data =
	| Point of Point.t
	| Empty
	| Node of t * t * t * t (* NW, NE, SE, SW *)

and t = Aabb.t * data

let empty bb = (bb,Empty)

let in_range (bb,_) p = Aabb.contains bb p

let rec insert (bb,d as qt) ~p:p =
	let open Direction in
	let generate_parent (bb,_ as qt) p =
		let new_bb = Aabb.(quadruple bb (dir_to_point bb p))
		in let (nw,ne,se,sw) = Aabb.quadrants new_bb in
		let new_d = match Aabb.dir_to_point bb p with
			| NW -> Node ((nw,Empty), (ne,Empty), qt, (sw,Empty))
			| NE -> Node ((nw,Empty), (ne,Empty), (se,Empty), qt)
			| SE -> Node (qt, (ne,Empty), (se,Empty), (sw,Empty))
			| SW -> Node ((nw,Empty), qt, (se,Empty), (sw,Empty))
		in (new_bb, new_d)
	in if in_range (bb,d) p
	then  match d with
		| Empty -> (bb, Point(p))
		| Node (nw,ne,se,sw) -> bb,(match Aabb.dir_to_point bb p with
			| NW -> Node (insert nw ~p:p, ne, se, sw)
			| NE -> Node (nw, insert ne ~p:p, se, sw)
			| SE -> Node (nw, ne, insert se ~p:p, sw)
			| SW -> Node (nw, ne, se, insert sw ~p:p)
			)
		| Point p2 -> if p2=p then qt
			else let (nw,ne,se,sw) = Aabb.quadrants bb 
			in insert (insert (bb, Node ((nw,Empty), (ne,Empty), (se,Empty), (sw,Empty))) ~p:p) ~p:p2
	else insert (generate_parent (bb,d) p) ~p:p

let rec singleton bb p = if Aabb.contains bb p
	then (bb,Point p)
	else singleton (Aabb.quadruple bb (Aabb.dir_to_point bb p)) p

let rec contains (bb,d) p =
	(* If the point is out of range, we can do this in constant time *)
	Aabb.contains bb p &&
	match d with
	| Point p2 -> p = p2
	| Empty -> false
	| Node (nw,ne,se,sw) ->
		contains (match (Aabb.dir_to_point bb p) with
			| Direction.NW -> nw
			| Direction.NE -> ne
			| Direction.SE -> se
			| Direction.SW -> sw)
			p

let rec to_list (_,d) =
	match d with
	| Point p -> [p]
	| Empty -> []
	| Node (nw,ne,se,sw) ->
		(to_list nw) @ (to_list ne) @ (to_list se) @ (to_list sw)
