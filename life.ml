(* The current algorithm uses the quadtree as a data store with log(N) lookup
 * that can handle arbitrarily large fields of sparse data. Since each cell
 * is stored near its neighbours in the tree, we can get O(1) lookups by making
 * the decision whether it lives or dies while iterating through the tree. For
 * any given subtree, we can determine whether points within it
 *	a) are live
 *	b) are dead
 *	c) need x more live neighbours to be live
 * This will entail having a function with the following signature:
 *	step : rules -> Qt.t -> (Qt.t * (Point.t * int) list)
 *
 * With patterns that shrink (or travel) by a substantial amount, this can lead
 * to several (possibly many) layer of the tree that are essentially empty. As
 * such, a compaction step should be added (maybe once every few iterations) to
 * prune the tree.
 *)
open Core.Std

module Qt = Quadtree

module Point_comp = Comparator.Make(struct
	type t = Point.t 
	let sexp_of_t (x,y) = sexp_of_pair sexp_of_int sexp_of_int (x,y)
	let compare = compare
end);;

type rules = {
	born : int list ;
	stay_alive : int list ;
	dies : int list ;
	} ;;

let neighbours (x,y) = [
	x-1,y+1;x+0,y+1;x+1,y+1;
	x-1,y+0;       x+1,y+0;
	x-1,y-1;x+0,y-1;x+1,y-1]
(*
let stays_alive r s p =
	let live_neighbours = List.filter (neighbours p) ~f:(Qt.contains s)
	in List.mem r.stay_alive (List.length live_neighbours)	

let born r s p =
	let live_neighbours = List.filter (neighbours p) ~f:(Qt.contains s) in
	List.mem r.born (List.length live_neighbours)
*)

(* step_aux takes rules and a quadtree, and advances the state by one step.
 * Since any points on the edge of the bounding box have an unknown number of
 * neighbours, we also return a pair of lists, one containing dead cells that
 * might come alive, and one containing live cells that might stay alive.
 *)
let rec step_aux (r:rules) ((bb,data) as s:Qt.t) : (Qt.t * Point.t list) =
	(* This function currently misbehaves because it doesn't distinguish
	points that might be born from those already alive. *)
	match data with
	| Point p -> (Qt.empty bb),List.append (if (Aabb.is_interior bb p) then [] else [p])
			(neighbours p |> List.filter ~f:(fun p -> not @@ (Aabb.is_interior bb p)))
	| Empty -> (Qt.empty bb),[]
	| Node (nw,ne,se,sw) ->
		let (nw',nw_rem) = step_aux r nw
		and (ne',ne_rem) = step_aux r ne
		and (se',se_rem) = step_aux r se
		and (sw',sw_rem) = step_aux r sw
		in let rem = List.fold [nw_rem;ne_rem;se_rem] ~init:sw_rem ~f:List.unordered_append
		in let point_list = List.map rem ~f:(fun p ->
			let n = neighbours p |> List.filter ~f:(Qt.contains s) |> List.length
			in print_endline ((Point.to_string p) ^ " has " ^ (string_of_int n) ^ " neighbours.") ;
			(p,n) )
		in let (live_points,maybe_point) =
			List.partition_tf point_list ~f:(fun (_,n) -> List.mem r.stay_alive n)
		in let maybe_point' = List.filter maybe_point ~f:(fun (_,n) -> not @@ List.mem r.dies n)
			|> List.map ~f:fst
		in (List.fold	live_points
				~init:(bb,Qt.Node (nw',ne',se',sw'))
				~f:(fun qt (p,_) -> Qt.insert qt ~p:p))
			,maybe_point'

(* Throw away any maybe points at the end. Maybe compaction should go here? *)
let step r s = let (result,_) = step_aux r s in result
	
let advance_draw r scale fg bg s =
	Graphics.set_color (Color.int_of_color bg) ;
	Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ()) ;
	Graphics.set_color (Color.int_of_color fg) ;
	Draw.quadtree scale s ;
	step r s

let rec draw_loop r scale delay fg bg s =
	let next_state = advance_draw r scale fg bg s in
	Unix.sleep delay ;
	draw_loop r scale delay fg bg next_state

let rec dump oc r steps state =
	Out_channel.output_lines oc [Quadtree.to_string state] ;
	match steps with
		| 0 -> ()
		| steps -> dump oc r (steps - 1) (step r state)

let from_file ic =
	let first_line = In_channel.input_line ic in
	match first_line with
	| None -> Quadtree.empty (Aabb.create (0,0) (256,256))
	| Some line -> Quadtree.of_string line
