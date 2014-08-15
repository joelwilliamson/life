open OUnit2

let () =
	let p1 = Point.create (2,3)
	and p2 = Point.create (5,6)
	and p3 = Point.create (-1,2)
	and p4 = Point.create (3,5)
	in let test_create_1 _ = assert_bool "Check x" ((Point.x p1) = 2)
	and test_create_2 _ = assert_bool "Check y" ((Point.y p2) = 6)
	and test_add _ = assert_bool "Check equal" ((Point.add p1 p2) = (Point.create (7,9)))
	and test_sub _ = assert_bool "Check subtraction" ((Point.sub p3 p4) = (Point.create (-4,-3)))
	and test_as _ = assert_bool "Check add_sub" ((Point.addsub p2 p3) = (Point.create (4,4)))
	and test_sa _ = assert_bool "Check sub_add" ((Point.subadd p1 p4) = (Point.create (-1,8)))
	and test_convert _ = assert_bool "Check string conversion" ((Point.to_string p2) = "5,6")
	and test_convert_back _ = assert_bool "Check string back convert" ((Point.of_string "5,6") = (5,6))

	in let suite = "Test Suite">:::[
		"Check x">::test_create_1
		;"Check y">::test_create_2
		;"Check add">::test_add
		;"Check sub">::test_sub
		;"Check add-sub">::test_as
		;"Check sub-add">::test_sa
		;"Check string conversion">::test_convert
		;"Check back conversion">::test_convert_back
		]
	in run_test_tt_main suite
