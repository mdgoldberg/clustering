open Core.Std

(* A module signature for comparing data types *)
module type COMPARABLE =
sig 
  
  (* Type *)
  type t
  
  (* Default of type, for array creation *)
  val default : t

  (* Comparison function *)
  val compare : t -> t -> Ordering.t

  (* Multiplication function*)
  val multiply : t -> t -> t

  (* Addition *)
  val add : t -> t -> t

  (* Prints specific types *)
  val print : t -> unit

  (*converts element to a float*)
  val float_of_t : t -> float

  (* inverse of above; converts flaot to t *)
  val t_of_float : float -> t

end

module type MATRIX = 
sig

  (*type of the elements*)
  type elt

  (*implementation of the matrix itself*)
  type t

  exception  Invalid_Dimensions

  (*creates a matrix out of list of lists*)
  val of_list : elt list list -> t

  (* creates a matrix with given dimensions. Initial values are undefined behavior *)
  val of_dimensions : (int * int) -> t

  (*multiplies two matrices together*)
  val multiply : t -> t -> t

  (*returns an element of the matrix *)
  val get : (int * int) -> t -> elt

  (* edits an element of the matrix and returns unit *)
  val set : (int * int) -> t -> elt -> unit

  (*returns the dimensions of an array*) 
  val dimensions : t -> (int * int)

  (*returns the smallest non-zero element of a matrix*)
  val minimum : t -> elt

  (*since matrices are abstract, this function prints them out*)
  val print : t  -> unit

  (* similarly to the above, we can use this for testing and printing *)
  val print_elt : elt -> unit

  (*returns all the elements of a given column *)
  val get_column : t -> int -> elt array

  (*turns elements into floats for purposes of stats module*)
  val float_of_elt : elt -> float

  (* inverse of the above transformation *)
  val elt_of_float : float -> elt

end

type cluster_args_t = | Kruskal of int
		      | Markov of int * int

(*signature for graph clustering algorithms:
 * Kruskal's Algorithm and Markov Process Clustering *)
module type CLUSTER =
  functor (Matrix: MATRIX) ->
  sig 
  (*The first argument is an option for the parameters of that
   *specific algorithm. If the algorithm doesn't take in any, just
   * pass in None. *)
  val cluster: cluster_args_t -> Matrix.t -> int list list
end

module type STATS =
  functor (Matrix: MATRIX) ->
  sig

  val avg_dist_single : Matrix.t -> int list-> float

  val avg_dist_all : Matrix.t -> int list list -> float list

  val dist_between_two : Matrix.t -> int list -> int list -> float

  val dist_between_all : Matrix.t -> int list list -> float list 
					     
end

module type TO_GRAPH = 
  functor (Matrix: MATRIX) ->
  sig

  val to_graph : 'a -> Matrix.t

  end

