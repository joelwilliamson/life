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
	} ;;

let neighbours (x,y) = [
	x-1,y+1;x+0,y+1;x+1,y+1;
	x-1,y+0;       x+1,y+0;
	x-1,y-1;x+0,y-1;x+1,y-1]

let stays_alive r s p =
	let live_neighbours = List.filter (neighbours p) ~f:(Qt.contains s)
	in List.mem r.stay_alive (List.length live_neighbours)	

let born r s p =
	let live_neighbours = List.filter (neighbours p) ~f:(Qt.contains s) in
	List.mem r.born (List.length live_neighbours)

let step r s =
	let live_points = Qt.to_list s in
	let neighbours = List.fold live_points ~init:[] ~f:(fun acc x ->
			(neighbours x) @ acc)
		|> Set.of_list ~comparator:Point_comp.comparator in
	let neighbours = Set.diff neighbours  (Set.of_list ~comparator:Point_comp.comparator live_points)
		|> Set.to_list in
	let stay_alive = List.filter live_points ~f:(stays_alive r s)
	and born = List.filter neighbours ~f:(born r s) in
	List.fold (stay_alive @ born) ~init:(Quadtree.empty (Aabb.create (0,0) (16,16)))
		~f:(fun acc x -> Quadtree.insert acc ~p:x)

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
