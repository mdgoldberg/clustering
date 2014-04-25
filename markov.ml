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
    let to_float t = t
  end

  module FloatMatrix : MATRIX = Matrix.ArrayMatrix(FloatCompare)

  let normalize (m : MatrixMod.t) : FloatMatrix.t =
    let (dimx, dimy) = MatrixMod.dimensions m in
    let newmat = FloatMatrix.of_dimensions (dimx,dimy) in
    let rec loop col =
      if col = dimy then newmat else
	MatrixMod.(
	  let elts = get_column m col in
	  let new_elts = Array.map elts
	    ~f:(fun e -> (float_of_elt e) /. Float.of_int (Array.length elts))
	  in 
	  Array.iteri new_elts ~f:(fun r e -> FloatMatrix.set (r,col) newmat e); (* PROBLEM: e IS OF TYPE float, NOT TYPE FloatMatrix.t *)
	  loop (col+1)
	)
    in 
    loop m 0

(* Functions to write:
 * normalize;
 * expand;
 * inflate;
 * has_converged (?); - look for repeats? could require a ton of space
 * interpret *)

  let cluster (args : 'a option) (m : Matrix.t) : int list list =
    let norm_matrix = normalize m in
    let rec iterate mat (* keep list of past matrices here? *) =
      if has_converged mat then
	interpret mat
      else 
	iterate (inflate (expand mat e) r)
    in iterate norm_matrix

end

