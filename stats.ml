open Core.Std
open Signatures
open Matrix

(*right now Stats only works with float matrices*)
module Stats (Matrix: MATRIX with type elt = float): STATS = 
  struct

    let avg_dist_single (m: Matrix.t) (clst: int list) : float =
      let len = List.length clst in
      let arr = Array.of_list clst in
      let sum = ref 0.0 in
      let ctr = ref 0.0 in
      for i = 0 to len - 1 do 
	for j = i to len - 2 do
	  sum := !sum +. Matrix.get (arr.(j + 1), arr.(j)) m;
	  ctr := !ctr +. 1.0
	done;
      done;
      !sum /. !ctr

    let avg_dist_all (m: Matrix.t) (clst: int list list) : float list= 
      failwith "Not implemented"

    let dist_between_two (m: Matrix.t) (cl1: int list) (cl2: int list) : float = 
      failwith "not implemented"

    let dist_between_all (m: Matrix.t) (clst: int list list) : float list =
      failwith "not implemented"
  end 

module FloatMatrix = ArrayMatrix(FloatCompare)
module S = Stats(FloatMatrix)

assert(S.avg_distance_single (FloatMatrix.of_list [[0;1;2];[1;0;3];[2;3;0]]) 
			      [0;1] = 1)

