open Core

module type MATRIX = 
sig

  (*type of the elements*)
  type elt

  (*implementation of the matrix itself*)
  type t

  (*creates a matrix out of list of lists*)
  val of_list : elt list list -> t
  
  (*multiplies two matrices together*)
  val multiply : t -> t -> t

  (*returns an element of the matrix *)
  val get : (int * int) -> t -> elt
end

(*signature for graph clustering algorithms:
 * Kruskal's Algorithm and Markov Process Clustering *)
module type CLUSTER =
sig
  (*The first argument is an option for the parameters of that
   *specific algorithm. If the algorithm doesn't take in any, just
   * pass in None. *)
  val cluster: 'a option -> 'b MATRIX.t -> 'b list list
end

module type STATS =
sig

  val avg_dist_single : 'a list-> float

  val avg_dist_all : 'a list list -> float list

  val dist_between_two : 'a list -> 'a list -> float

  val dist_between_all : 'a list list -> float list list
					     
end

module type TO_GRAPH = 
sig

  val to_graph : 'a -> 'b list list

end

