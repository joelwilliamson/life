(** Details the algorithm used to decided whether a given cell is alive. The
  * number of live neighbours is counted and if the cell is live and
  * n \in stay_alive, or if the cell is dead and n \in born then the cell will
  * be alive in the next generation.
  **)
type rules = {
	born : int list ;
	stay_alive : int list ;
	} ;;

(** Create the next state given the current state, under the given rules **)
val step : rules -> Quadtree.t -> Quadtree.t

(** Draw the current state at the given scale and return the next one **)
val advance_draw : rules -> int -> Color.t -> Color.t -> Quadtree.t -> Quadtree.t

(** Draw many states, each following the previous, at the given scale **)
val draw_loop : rules -> int -> int -> Color.t -> Color.t -> Quadtree.t -> 'a

(** Run the simulation for a given number of steps, outputting one step per line **)
val dump: out_channel -> rules -> int -> Quadtree.t -> unit

(** Get a state from the first line of an input channel **)
val from_file : in_channel -> Quadtree.t
