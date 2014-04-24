open Core.Std

module type MATRIX = 
sig

  (*type of the elements*)
  type elt

  (*implementation of the matrix itself*)
  type t

  exception  Invalid_Dimensions

  (*creates a matrix out of list of lists*)
  val of_list : elt list list -> t
  
  (*multiplies two matrices together*)
  val multiply : t -> t -> t

  (*returns an element of the matrix *)
  val get : (int * int) -> t -> elt

  (*returns the dimensions of an array*) 
  val dimensions : t -> (int * int)

  (*returns the smallest non-zero element of a matrix*)
  val minimum : t -> elt

  (*since matrices are abstract, this function prints them out*)
  val print : t  -> unit

  (* similarly to the above, we can use this for testing and printing *)
  val print_elt : elt -> unit

  (*returns all the elements of the column, 
   except the one on the main diagonal*)
  val get_column : t -> int -> elt list

end

(*signature for graph clustering algorithms:
 * Kruskal's Algorithm and Markov Process Clustering *)
module type CLUSTER =
  functor (Matrix: MATRIX) ->
  sig 
  (*The first argument is an option for the parameters of that
   *specific algorithm. If the algorithm doesn't take in any, just
   * pass in None. *)
  val cluster: 'a option -> Matrix.t -> int list list
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

