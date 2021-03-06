open Core.Std

(* A module signature for comparing data types *)
module type COMPARABLE =
sig 
  
  (* Type *)
  type t
  
  (* zero of type, used for array creation (Invariant) *)
  val zero : t

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

  val zero : elt

  (*creates a matrix out of list of lists*)
  val of_list : elt list list -> t

  (* creates a matrix with given dimensions. Initial values are
     undefined behavior *)
  val of_dimensions : (int * int) -> t

  (*multiplies two matrices together*)
  val multiply : t -> t -> t

  (* map function over matrix *)
  val map : t -> f:(elt -> elt) -> t

  (*returns an element of the matrix *)
  val get : (int * int) -> t -> elt

  (* edits an element of the matrix and returns unit *)
  val set : (int * int) -> t -> elt -> unit

  (*returns the dimensions of an array*) 
  val dimensions : t -> (int * int)

  (*returns the smallest non-zero element of a matrix*)
  val maximum : t -> elt

  (*since matrices are abstract, this function prints them out*)
  val print : t  -> unit

  (* similarly to the above, we can use this for testing and printing *)
  val print_elt : elt -> unit

  (*returns all the elements of a given column *)
  val get_column : t -> int -> elt array

  (* returns all elements in a given row *)
  val get_row : t -> int -> elt array

  (*turns elements into floats for purposes of stats module*)
  val float_of_elt : elt -> float

  (* inverse of the above transformation *)
  val elt_of_float : float -> elt

  (* Compares two elts *)
  val compare_elts : elt -> elt -> int

end

type cluster_args_t = | Kruskal of int
		      | Markov of int * float * bool

(*signature for graph clustering algorithms:
 * Kruskal's Algorithm and Markov Process Clustering *)
module type CLUSTER =
  functor (MatrixMod : MATRIX) ->
  sig 

    (* The first argument is for the parameters of that
     * specific algorithm.
     * The second argument is the matrix, and the third argument
     * is a verbose option. *)
    val cluster: cluster_args_t -> MatrixMod.t -> int list list
end

module type STATS =
  functor (Matrix : MATRIX) ->
  sig

  (* calculates the average distance between nodes in a cluster
   * would ideally be small for good clustering algorithms 
   * returns none if there are no connections between the nodes *)
  val avg_dist_single : Matrix.t -> int list -> float option

  (* does the same thing as the above but for multiple clusters *)
  val avg_dist_all : Matrix.t -> int list list -> float option list

  (* calculates distances between clusters
   * would ideally be large for good clustering algorithms 
   * returns None if the two clusters are not connected *)
  val dist_between_two : Matrix.t -> int list -> int list -> float option

  (* calculates the distance between all pairs of clusters *)
  val dist_between_all : Matrix.t -> int list list -> float option list 
					     
end

module type TO_GRAPH = 
  functor (Matrix: MATRIX) ->
sig

  (* takes in a list of points and outputs the transition matrix *)
  val to_graph : float list list -> Matrix.t

end

