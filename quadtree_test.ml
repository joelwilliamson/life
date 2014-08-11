open OUnit2

let qt = Quadtree.(empty (Aabb.create (3,5) (2,8))
	|> insert ~p:(2,6)
	|> insert ~p:(4,6)
	|> insert ~p:(4,0)
	|> insert ~p:(2,4)
	|> insert ~p:(23,7)
	|> insert ~p:(-6,2)
	|> insert ~p:(0,0))

let single = Quadtree.singleton (Aabb.create (0,0) (1,1)) (3,5)

let make_bool_test str test = str>::fun x -> assert_bool str test

let single_test_1 = make_bool_test "Single contains point" (Quadtree.contains single (3,5))
let single_test_2 = make_bool_test "Single contains nothing else" ((Quadtree.contains single (5,3)) |> not)

let test_suite = "Quadtree Test Suite">:::[
	single_test_1
	;single_test_2
	]

let () = run_test_tt_main test_suite
