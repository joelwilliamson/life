open OUnit2

let qt = Quadtree.(empty (Aabb.create (3,5) (8,8))
	|> insert ~p:(2,6)
	|> insert ~p:(4,6)
	|> insert ~p:(4,0)
	|> insert ~p:(2,4)
	|> insert ~p:(23,7)
	|> insert ~p:(-6,2)
	|> insert ~p:(0,0)
	)

let single = Quadtree.singleton (Aabb.create (0,0) (1,1)) (3,5)

let make_bool_test str test  = str>::fun _ -> assert_bool str test
let make_false_test str test = str>::fun _ -> assert_bool str (test |> not)

let single_test_1 = make_bool_test "Single contains point" (Quadtree.contains single (3,5))
let single_test_2 = make_false_test "Single contains nothing else" (Quadtree.contains single (5,3))
let big_tree_check_1 = make_bool_test "Tree Containment 1" (Quadtree.contains qt (2,6))
let big_tree_check_2 = make_bool_test "Tree Containment 2" (Quadtree.contains qt (4,0))
let big_tree_check_3 = make_bool_test "Tree Containment 3" (Quadtree.contains qt (23,7))
let big_tree_check_4 = make_bool_test "Tree Containment 4" (Quadtree.contains qt (0,0))
let big_tree_check_neg_1 = make_false_test "Tree Non-Contain 1" (Quadtree.contains qt (1,0))
let big_tree_check_neg_2 = make_false_test "Tree Non-Contain 2" (Quadtree.contains qt (4,5))
let singleton_list = make_bool_test "Singleton to List" ((Quadtree.to_list qt) = [3,5])
let empty_list = make_bool_test "Empty to List"
		((Quadtree.to_list (Quadtree.empty (Aabb.create (3,4) (2,2)))) = [])
let big_tree_list = make_bool_test "Big tree to List"
		((Quadtree.to_list qt |> List.sort compare) =
		List.sort compare [2,6;4,6;4,0;2,4;23,7;-6,2;0,0])

let test_suite = "Quadtree Test Suite">:::[
	single_test_1
	;single_test_2
	;big_tree_check_1
	;big_tree_check_2
	;big_tree_check_3
	;big_tree_check_4
	;big_tree_check_neg_1
	;big_tree_check_neg_2
	]

let () = run_test_tt_main test_suite
