open Core.Std
open Signatures
open Matrix

module Markov : CLUSTER =
  functor (MatrixMod : MATRIX) -> 
struct

  module FloatCompare : COMPARABLE with type t = float =
  struct
    type t = float
    let default = 0.0
    let compare a b = 
      let diff = (a -. b) in
      if diff > 0. then Greater else
	if diff = 0. then Equal else Less
    let multiply a b = a *. b
    let add a b  = a +. b
    let print t = print_float t
    let float_of_t t = t
    let t_of_float f = f
  end

  module FloatMatrix : MATRIX = Matrix.ArrayMatrix(FloatCompare)

  let sumelts (arr : MatrixMod.elt array) : float =
    Array.fold_right arr
      ~f:(fun x r -> (MatrixMod.float_of_elt x) +. r) ~init:0.

  let sumfloats (arr : float array) : float =
    Array.fold_right arr ~f:(+.) ~init:0.

  let normalize (m : MatrixMod.t) : FloatMatrix.t =
    let (dimx, dimy) = MatrixMod.dimensions m in
    let newmat = FloatMatrix.of_dimensions (dimx,dimy) in
    let rec loop col =
      if col = dimy then newmat else
	MatrixMod.(
	  let elts = get_column m col in
	  let sum = sumelts elts in
	  let new_elts = Array.map elts
	    ~f:(fun e -> (float_of_elt e) /. sum)
	  in 
	  Array.iteri new_elts ~f:(fun r e -> FloatMatrix.set
	    (r,col) newmat (FloatMatrix.elt_of_float e));
	  loop (col+1)
	)
    in 
    loop 0

  let expand m e =
    let rec loop newmat count =
      if count = 1 then m else
	loop (FloatMatrix.multiply newmat m) (count-1)
    in loop m e

  let inflate m r =
    let (_,numcols) = FloatMatrix.dimensions m in
    let rec loop newmat origmat col =
      if col = numcols then newmat else
	let elts = FloatMatrix.get_column newmat col in
	let eltssq = Array.map elts
	  ~f:(fun e -> (FloatMatrix.float_of_elt e) ** r) in
	let sum = sumfloats eltssq in
	let newelts = Array.map eltssq ~f:(fun e -> e /. sum) in
	Array.iteri newelts ~f:(fun r e -> FloatMatrix.set (r,col) newmat
	  (FloatMatrix.elt_of_float e));
	loop newmat origmat (col+1)
    in loop m m 0


  let has_converged m = true (* This is gonna suck *)

  let interpret m =
    let (numrows,_) = FloatMatrix.dimensions m in
    let rec loop row results =
      if row = numrows then results else
	failwith "not implemented"
    in loop 0 []

(* Functions to write:
 * normalize;
 * expand;
 * inflate;
 * has_converged (?); - look for repeats? could require a ton of space
 * interpret *)

  let cluster (args : cluster_args_t) (m : MatrixMod.t) : int list list =
    let norm_matrix = normalize m in
    let (e,r) = match args with
      | Kruskal _ -> failwith "Wrong argument type"
      | Markov (e,r) -> (e,r)
    in 
    let rec iterate mat i (* keep list of past matrices here? *) =
      if i = 0 then
	interpret mat
      else 
	iterate (inflate (expand mat e) r) (i-1)
    in iterate norm_matrix 10

end

