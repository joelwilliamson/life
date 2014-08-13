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


let r = { born = [3]; stay_alive = [2;3]}

let advance_draw s =
	Graphics.set_color 0xFFFFFF ;
	Graphics.fill_rect 0 0 1000 1000 ;
	Graphics.set_color 0xFF ;
	Draw.quadtree 1 s ;
	Unix.sleep 0 ;
	step r s

let rec draw_loop s =
	advance_draw s |> draw_loop

let () =
	Graphics.open_graph " 1000x1000" ;
	Graphics.set_color 0x00FF00 ;
	let state = Quadtree.(empty (Aabb.create (0,0) (128,128))
		|> insert ~p:(0,1)
		|> insert ~p:(1,1)
		|> insert ~p:(0,0)
		|> insert ~p:(-1,0)
		|> insert ~p:(0,-1)
		) in
	draw_loop state
