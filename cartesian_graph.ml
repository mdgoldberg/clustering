open Core.Std
open Matrix
open Signatures


module Cartesian : TO_GRAPH = 
  functor (Matrix: MATRIX) ->
  struct

  (* INVARIANTS: 
   *  1 - All the elements of the list must be of the same dimension
   *  2 - The list must contain more than 1 element
   *      *)
  
  
  (*function to calculate distances*)
  let rec distance_fun lst1 lst2 acc = 
    match lst1, lst2 with
    | hd1 :: tl1, hd2 :: tl2 -> 
       let term = (hd1 -. hd2) ** 2. in
       distance_fun tl1 tl2 (acc +. term)
    | _ :: _, [] 
    | [], _ :: _ -> failwith "ERROR: Points of different dimensions"
    | [], [] -> acc ** 0.5 ;;

  let to_graph (points_lst : float list list) : Matrix.t =
    

    (*calculate size of the matrix*)
    let len = (List.length points_lst) in

    (*iniitialize the result matrix*)
    let res = Matrix.of_dimensions (len, len) in
    
    
    (* array to iterate to create distances*)
    let arr =  Array.of_list points_lst in

    (* loop to iterate over list of points *)
    for i = 0 to len - 1 do 
      for j = i + 1 to len - 1 do

	let new_elt = Matrix.elt_of_float (distance_fun arr.(i) arr.(j) 0.) in

	(* for cartesian points the |distance| from a -> b is the same as b -> a
	 * therefore the matrix is symmetric! *)
	Matrix.set (j, i) res new_elt;
	Matrix.set (i, j) res new_elt
      done;
    done; 
    res
				 
end

module FloatMatrix = ArrayMatrix(FloatCompare)

module Cart = Cartesian(FloatMatrix)

let test_points = [[0.;0.]; [0.; 3.]; [4.; 0.]];;
let graph = Cart.to_graph test_points ;;

FloatMatrix.print graph 
