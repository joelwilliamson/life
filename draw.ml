(* This module will currently assume it is drawing onto a 1000x1000 buffer,
 * representing a 100x100 grid centred at (0,0)
 *)

open Core.Std

let point scale (x,y) =
	let x = x * scale + 500
	and y = y * scale + 500 in
	Graphics.fill_rect x y scale scale

let quadtree scale t =
	List.iter ~f:(point scale) (Quadtree.to_list t)
