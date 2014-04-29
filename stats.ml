open Core.Std
open Signatures
open Matrix

module Stats: STATS = 
  functor(Matrix: MATRIX) ->
  struct

    let avg_dist_single (m: Matrix.t) (clst: int list) : float =
      let len = List.length clst in
      let arr = Array.of_list clst in
      let sum = ref 0.0 in
      let ctr = ref 0.0 in
      for i = 0 to len - 1 do 
	for j = i to len - 2 do
	  sum := !sum +. Matrix.float_of_elt (Matrix.get (arr.(j + 1), arr.(j)) m);
	  ctr := !ctr +. 1.0
	done;
      done;
      !sum /. !ctr
      

    let avg_dist_all (m: Matrix.t) (clst: int list list) : float list= 
      List.map clst (fun x -> avg_dist_single m x) 

    let dist_between_two (m: Matrix.t) (cl1: int list) (cl2: int list) : float = 
      let arr1 = Array.of_list cl1 in
      let arr2 = Array.of_list cl2 in
      let len1 = List.length cl1 in
      let len2 = List.length cl2 in
      let sum = ref 0. in
      let ctr = ref 0. in
      for i = 0 to len1 - 1 do
	for j = 0 to len2 - 1 do
	  sum := !sum +. Matrix.float_of_elt (Matrix.get (arr2.(j), arr1.(i)) m);
	  ctr := !ctr +. 1.
	done;
      done;
      !sum /. !ctr


    let dist_between_all (m: Matrix.t) (clst: int list list) : float list =
      let arr = Array.of_list clst in
      let len1 = List.length clst in
      let res = ref [] in
      for i = 0 to len1 - 1 do
	for j = i + 1 to len1 - 1 do
	   res := (dist_between_two m arr.(i) arr.(j)) :: !res
	done;
      done;
      !res
      
  end 

module FloatMatrix = ArrayMatrix(FloatCompare)
module S = Stats(FloatMatrix)


assert(S.avg_dist_single (FloatMatrix.of_list [[0.;1.;2.];[1.;0.;3.];[2.;3.;0.]]) 
			     [0;1] = 1.0)
let tm = FloatMatrix.of_list [[0.;1.;1.;1.;1.];[1.;0.;2.;2.;2.];[2.;2.;0.;3.;3.];[3.;3.;3.;0.;4.];[4.;4.;4.;4.;0.]]
assert(S.avg_dist_all tm [[0;1;2];[3;4]] = [5./.3.; 4.])

assert(S.dist_between_two tm [0;1;2] [3;4] = (21. /. 6.))

assert(S.dist_between_all tm [[0;1;2];[4];[3]] = [4. ;3. ;4.])
