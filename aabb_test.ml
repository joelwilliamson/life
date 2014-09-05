open OUnit2

let b1 = Aabb.create (2,2) (2,4)
and b2 = Aabb.create (3,5) (2,8)
and b3 = Aabb.create (-3,-3) (1,1)
and zero_h = Aabb.create (0,0) (1,0)
and zero_v = Aabb.create (0,0) (0,1)

let contains_1 _ = assert_bool "b1 contains origin" (Aabb.contains b1 (1,0))
let contains_2 _ = assert_bool "b2 doesn't contain origin" (Aabb.contains b2 (0,0)|> not)
let intersect_1 _ = assert_bool "b1 & b2 intersect" (Aabb.intersect b1 b2)
let intersect_2 _ = assert_bool "b1 & b3 don't intersect" (Aabb.intersect b1 b3 |> not)
let dir_1 _ = assert_equal ~msg:"b1 -> (1,3) = NW" (Aabb.dir_to_point b1 (1,3)) NW
let dir_2 _ = assert_equal ~msg:"b1 -> (3,3) = NE" (Aabb.dir_to_point b1 (3,3)) NE
let dir_3 _ = assert_equal ~msg:"b1 -> (3,1) = SE" (Aabb.dir_to_point b1 (3,1)) SE
let dir_4 _ = assert_equal ~msg:"b1 -> (1,1) = SW" (Aabb.dir_to_point b1 (1,1)) SW
let quad_1 _ = assert_equal ~msg:"quadruple 1" (Aabb.quadruple b1 NE) (Aabb.create (4,6) (4,8))
let quad_2 _ = assert_equal ~msg:"quadruple 2" (Aabb.quadruple b2 SE) (Aabb.create (5,-3) (4,16))
let quadrants_1 _ = assert_equal ~msg:"quadrants" (Aabb.quadrants b1)
	((Aabb.create (1,4) (1,2)),
	(Aabb.create (3,4) (1,2)),
	(Aabb.create (3,0) (1,2)),
	(Aabb.create (1,0) (1,2)))
let quadrant_dir _ = assert_bool "The quadrant in the direction of p contains p"
	(Aabb.contains
		(let nw,ne,se,sw = Aabb.quadrants b1 in
		match Aabb.dir_to_point b1 (2,3) with
		| NW -> nw
		| NE -> ne
		| SE -> se
		| SW -> sw) (2,3))
let zero_length_interval_1 _ = assert_bool "Horizontal interval contains origin"
	((Aabb.contains zero_h (0,0)) && (Aabb.contains zero_h (-1,0)))
let zero_length_interval_2 _ = assert_bool "Vertical interval contains origin"
	((Aabb.contains zero_v (0,0)) && (Aabb.contains zero_v (0,-1)))

let interior_1 _ = assert_bool "origin is not interior to b1" (not (Aabb.is_interior b1 (0,0)))
let interior_2 _ = assert_bool "center is interior to box" (Aabb.is_interior b1 (2,2))


let () =
	 run_test_tt_main ("Aabb Test Suite">:::[
		"contains_1">::contains_1
		;"contains_2">::contains_2
		;"intersect_1">::intersect_1
		;"intersect_2">::intersect_2
		;"dir_1">::dir_1
		;"dir_2">::dir_2
		;"dir_3">::dir_3
		;"dir_4">::dir_4
		;"quad_1">::quad_1
		;"quad_2">::quad_2
		;"quadrants">::quadrants_1
		;"quadrants2">::quadrant_dir
		;"zero_size1">::zero_length_interval_1
		;"zero_size2">::zero_length_interval_2
		;"interior_1">::interior_1
		;"interior_2">::interior_2
		])
