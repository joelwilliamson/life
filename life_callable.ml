open Core
open Core.Std

let (>>) f g x = f x |> g
let digit_to_int = Char.to_int >> (+) (-48)

let spec =
	let open Command.Spec in
	let int_list = Arg_type.create (fun str ->
		String.to_list str
		|> List.map ~f:digit_to_int) in
	empty
	+> flag "--scale" (optional_with_default 5 int) ~doc:"scale The number of pixels per cell"
	+> flag "--b" (optional_with_default [3] int_list) ~doc:"born Number of neighbours to transition dead->alive"
	+> flag "--s"  (optional_with_default [2;3] int_list) ~doc:"survival Number of neighbours to survive"
	+> flag "--t" (optional_with_default 1 int) ~doc:"delay Time between frames"
	+> flag "--write-to-file" (optional file) ~doc:"dumpfile Write the simulation to file"
	+> flag "--steps" (optional_with_default 100 int) ~doc:"steps Simulate this many steps in write mode"
	+> anon ("filename" %: file)

let command =
	Command.basic
		~summary:"Simulate the game of life"
		~readme:(fun () -> "More detailed info")
		spec
		(fun scale born survive delay outfile steps filename () ->
			let state = In_channel.create filename |> Life.from_file in 
			match outfile with
			| None ->
				Graphics.open_graph " 1000x1000" ;
				Life.draw_loop {born;stay_alive=survive} scale delay state
			| Some filename ->
				let oc = Out_channel.create filename in
				Life.dump oc {born;stay_alive=survive} steps state)

let () =
	Command.run ~version:"1.0" command
