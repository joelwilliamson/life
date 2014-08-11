open Core.Std

let () =
	let open OUnit2 in
	let open Direction in
	let forward x = to_ne x |> from_ne
	and backward x = from_ne x |> to_ne
	in let forward x _ = assert_bool "Forward test" ((forward x) = x)
	and backward x _ = assert_bool "Backward test" ((backward x) = x)
	in let suite = "Direction Test Suite">:::
		((List.map ~f:(fun x -> backward x |> (>::) "Backward test")
			[(true,true);true,false;false,false;false,true])
		@(List.map ~f:(fun x -> forward x |> (>::) "Forward test")
			[NW;NE;SE;SW]))
	in run_test_tt_main suite
