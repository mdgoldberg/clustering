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

  module FloatMatrix = ArrayMatrix(FloatCompare)

  let normalize (m : MatrixMod.t) : FloatMatrix.t =
    let sumelts = Array.fold_right ~init:0.
      ~f:(fun x r -> (MatrixMod.float_of_elt x) +. r) in
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
      if count = 1 then newmat else
	loop (FloatMatrix.multiply newmat m) (count-1)
    in loop m e

  let inflate m r =
    let sumfloats = Array.fold_right ~f:(+.) ~init:0. in
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

  let interpret m =
    let (numrows,_) = FloatMatrix.dimensions m in
    let rec loop row results =
      if row = numrows then results else
	FloatMatrix.(
	  let elts = get_row m row in
	  let clust = Array.foldi elts ~init:[] ~f:(fun i res e ->
	    if float_of_elt e > 0. then i :: res else res) in
	  loop (row+1) (clust :: results)
	)
    in List.filter_map (loop 0 []) ~f:(fun cl ->
      if (cl = []) then None else Some (List.sort ~cmp:(-) cl))

  let last_matrix = ref (FloatMatrix.of_list [[]])

  let has_converged m = m = (!last_matrix)

(* Functions to write:
 * has_converged
 *)

  let cluster (args : cluster_args_t) (m : MatrixMod.t) : int list list =
    let (e,r) = match args with
      | Kruskal _ -> failwith "Wrong argument type"
      | Markov (e,r) -> (e,r)
    in 
    let rec iterate mat =
      if has_converged mat then
	begin 
	  FloatMatrix.print mat;
	  interpret mat
	end 
      else 
	let newm = inflate (expand mat e) r in
	FloatMatrix.print newm;
	last_matrix := mat;
	iterate newm
    in iterate (normalize m)

let _ = ();;

end

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
(*
line 1105

Buggy test code - type checking not successful

module FloatMatrix : MATRIX = Matrix.ArrayMatrix(FloatCompare)

module FloatMarkov : CLUSTER = Markov(FloatMatrix)

FloatMarkov.cluster (Markov (2,2.)) (FloatMatrix.of_list [[1;1;1;1];[1;1;0;1];[1;0;1;0];[1;1;0;1]])
*)
