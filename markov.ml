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
	  Array.iteri new_elts ~f:(fun r e -> FloatMatrix.set
	    (r,col) newmat (FloatMatrix.elt_of_float e));
	  loop (col+1)
	)
    in 
    loop 0

  let expand m e = m
  let inflate m r = m
  let has_converged m = true
  let interpret m = [[0];[1]]

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
    let rec iterate mat (* keep list of past matrices here? *) =
      if has_converged mat then
	interpret mat
      else 
	iterate (inflate (expand mat e) r)
    in iterate norm_matrix

end

