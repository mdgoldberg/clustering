open Core.Std
open Signatures
open Matrix

module Markov : CLUSTER =
  functor (MatrixMod : MATRIX) ->
struct

  module FloatCompare : COMPARABLE with type t = float =
  struct
    type t = float
    let zero = 0.0
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
      FloatMatrix.print mat;
      if has_converged mat then
	begin 
	  interpret mat
	end 
      else 
<<<<<<< HEAD
	begin 
	  let newm = inflate (expand mat e) r in
	  last_matrix := mat;
	  iterate newm
	end 
=======
	let newm = inflate (expand mat e) r in
	last_matrix := mat;
	iterate newm
>>>>>>> e015eb0db2adad4eeb10c60c553435698225795b
    in iterate (normalize m)

end

module IntCompare : COMPARABLE with type t = int =
struct
  type t = int
  let zero = 0
  let compare a b = Ordering.of_int (a-b)
  let multiply a b = a * b
  let add a b  = a + b
  let print t = print_int t
  let float_of_t t = Float.of_int t
  let t_of_float f = Int.of_float f
end

module IntMatrix = ArrayMatrix(IntCompare)

module IntMarkov = Markov(IntMatrix)

let test = IntMarkov.cluster (Markov (2,2.)) (IntMatrix.of_list [[0;1;0];
								 [1;0;100];
								 [0;100;0]])

let print_lists lsts =
  let print_list =
    List.iter ~f:(fun e -> print_int e; print_string " ")
  in List.iter ~f:(fun lst -> print_list lst; print_string "\n") lsts

let _ = print_lists test

