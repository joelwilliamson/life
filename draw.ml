(* This module will currently assume it is drawing onto a 1000x1000 buffer,
 * representing a 100x100 grid centred at (0,0)
 *)

open Core.Std

let point (x,y) =
	let x = x * 5 + 500
	and y = y * 5 + 500 in
	Graphics.fill_rect x y 5 5

let quadtree t =
	List.iter ~f:point (Quadtree.to_list t)
