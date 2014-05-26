open Core.Std
open Signatures
open Matrix
open Cartesian_graph 

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

  (* Adds self-loops, takes inverse of non-zero elts, and converts to
   * FloatMatrix.t *)
  let toFloatMat (m : MatrixMod.t) : FloatMatrix.t =
    let (dimx, dimy) = MatrixMod.dimensions m in
    let newmat = FloatMatrix.of_dimensions (dimx,dimy) in
    let rec loop col =
      if col = dimy then newmat else
	MatrixMod.(
	  let elts = get_column m col in
	  let new_elts = Array.map elts ~f:(fun e ->
	    let fe = float_of_elt e in
	    if fe = 0. then fe else 1. /. fe) in
	  Array.iteri new_elts ~f:(fun r e ->
	    FloatMatrix.set (r,col) newmat (FloatMatrix.elt_of_float e));
	  loop (col+1)
	)
    in 
    let newmat = loop 0 in
    let rec add_loops col =
      if col = dimy then newmat else
	FloatMatrix.(
	  let elts = get_column newmat col in
	  let max arr = Array.fold_right arr ~f:max ~init:(arr.(0)) in
	  Array.iteri elts ~f:(fun r _ ->
	    if r = col then
	      begin 
		set (r,col) newmat 0.0;
		set (r,col) newmat (max elts)
	      end 
	    else ());
	  add_loops (col+1)
	)
    in add_loops 0

  (* Converts to a left stochastic matrix, where each column sums to 1 *)
  let normalize (m: FloatMatrix.t) : FloatMatrix.t =
    let (_, dimy) = FloatMatrix.dimensions m in
    let sumfloats = Array.fold_right ~f:(+.) ~init:0. in
    let rec loop col =
      if col = dimy then m else
	FloatMatrix.(
	  let elts = get_column m col in
	  let sum = sumfloats elts in
	  let new_elts = Array.map elts ~f:(fun e -> e /. sum) in
	  Array.iteri new_elts ~f:(fun r e -> set (r,col) m e);
	  loop (col+1)
	)
    in 
    loop 0

  (* Multiplies m by itself e times *)
  let expand m e =
    let rec loop newmat count =
      if count = 1 then newmat else
	loop (FloatMatrix.multiply newmat m) (count-1)
    in loop m e

  (* Inflates the matrix by taking each elt to the r power, then normalizes the
     result *)
  let inflate m r =
    let newmat = FloatMatrix.map m ~f:(fun e -> e ** r) in
    normalize newmat

  (* Interprets the final matrix to output the int list list of clusters *)
  let interpret m =
    let (numrows,_) = FloatMatrix.dimensions m in
    let rec unique (xs : 'a list) : 'a list =
      match xs with
      | [] -> []
      | hd::tl ->
	let tl' = unique tl in
	if List.mem tl' hd then tl' else hd::tl'
    in 
    let rec loop row results =
      if row = numrows then results else
	FloatMatrix.(
	  let elts = get_row m row in
	  let clust = Array.foldi elts ~init:[] ~f:(fun i res e ->
	    if float_of_elt e > 0.01 then i :: res else res) in
	  loop (row+1) (clust :: results)
	)
    in unique (List.filter_map (loop 0 []) ~f:(fun cl ->
      if (cl = []) then None else Some (List.sort cl ~cmp:(-))))

(*
Decided against this approach and instead for the chaos approach.
  let last_matrix = ref (FloatMatrix.of_list [[]])
  let has_converged m = m = (!last_matrix)
*)

(* Calculates the chaos of a matrix. Lower chaos means closer to final state *)
  let chaos m : float = 
    let (_,numcols) = FloatMatrix.dimensions m in
    let rec chaoses col acc =
      if col = numcols then acc else
	FloatMatrix.(
	  let elts = get_column m col in
	  let max = Array.fold_right elts ~init:elts.(0)
	    ~f:(fun x r -> if compare_elts x r >= 0 then x else r) in
	  let sumsq = Array.fold_right elts ~init:0.0
	    ~f:(fun x r -> (x ** 2.) +. r) in
	  let colchaos = max -. sumsq in
	  chaoses (col+1) (colchaos :: acc)
	)
    in 
    let chaos_lst = chaoses 0 [] in
    match chaos_lst with
    | hd :: tl -> List.fold_left tl ~init:hd
      ~f:(fun r x -> if FloatMatrix.compare_elts x r > 0 then x else r)
    | [] -> failwith "empty chaos list"
    

  (* The algorithm that brings it all together *)
  let cluster (args : cluster_args_t) (m : MatrixMod.t) : int list list =
    let (e,r,verbose) = match args with
      | Kruskal _ -> failwith "Wrong argument type"
      | Markov (e,r,verbose) -> (e,r,verbose)
    in 
    let rec iterate mat =
      if verbose then
	begin
	  Printf.printf "chaos: %f\n" (chaos mat);
	  FloatMatrix.print mat
	end 
      else ();
      if chaos mat < 0.001 then
	  interpret mat
      else 
	begin
	  let newm = inflate (expand mat e) r in
	  iterate newm
	end 
    in
    iterate (normalize (toFloatMat m))

end

(*
TEST CODE - UNCOMMENT TO RUN IF NECESSARY 

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

module IntToGraph = Cartesian(IntMatrix)

let test = IntMarkov.cluster (Markov (2,3.))
  (IntToGraph.to_graph [[1.;2.]; [2.;1.]; [1.;1.];
			[6.;7.]; [7.;6.]; [6.;6.]]);;
  
let print_lists lsts =
  let print_list =
    List.iter ~f:(fun e -> print_int e; print_string " ")
  in List.iter ~f:(fun lst -> print_list lst; print_string "\n") lsts

let _ = print_lists test
*)
