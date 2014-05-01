open Core.Std
open Matrix
open Signatures


module Kruskal : CLUSTER = 
	functor (M: MATRIX) ->
struct
  type elt = M.elt

  let edge_compare (x : (elt * int * int)) (y : (elt * int * int)) = 
    let (a, _, _) = x in
    let (b, _, _) = y in
    M.compare_elts a b

  let make_edgelist (m : M.t) : ((elt * int * int) Heap.t) =
    let q = Heap.create ~cmp: edge_compare () in
    let (a, b) = M.dimensions m in
    let size = max a b in
    let _ =
      for i = 0 to size - 1 do
	for j = 0 to size - 1 do 
	  if (M.get (i, j) m) = M.zero
	  then ()
	  else Heap.add q (M.get (i, j) m, i, j)
	done
      done 
    in
    q
  
  let make_forest (m: M.t) (size : int): int list list = 
    let rec loop (s: int list list) (n : int) =
      if n < 0
      then s
      else loop ([size] :: s) (n - 1)
    in
    loop [[]] size
	 
  let is_spanning (trees: int list list) (size: int) : bool =
    let rec sizes (n : int) =
      match n with
      | 0 -> [0]
      | x -> n :: (sizes x) in
    let xs = List.concat trees in
    (List.fold ~f: (+) ~init: 0 xs) = (List.fold_left ~f: (+) ~init: 0 (sizes size))
		
  let cluster (args: cluster_args_t) (m : M.t) : int list list =
    let n = match args with
      | Kruskal x -> x
      | Markov _ -> failwith "wrong argument type"
    in 
    let (size, _) = M.dimensions m in
    let edges = make_edgelist m in 
    let forest = make_forest m (size - 1) in
    let links = 0 in
    let max_links = size - n in
    let spanning (forest: int list list) (edges: (elt * int * int) Heap.t) : int list list = 
      let rec while_loop (forest': int list list) (edges': (elt * int * int) Heap.t) (links': int) : int list list =
	if Heap.is_empty edges' || is_spanning forest' (size - 1) || (links' >= max_links)
	then forest'
	else
	(match Heap.top edges' with
	 | None -> let _ = Heap.remove_top edges' in 
		   while_loop forest' edges' links'
	 | Some (_, v1, v2)-> (match List.find forest' (List.mem [[v1]]) with
			       | None -> let _ = Heap.remove_top edges' in 
					 while_loop forest' edges' links'
			       | Some (xs) -> if (List.mem xs v2) 
						   (*a cycle would occur*)
					      then let _ = Heap.remove_top edges' in 
						   while_loop forest' edges' links'
					      else let Some ys = List.find forest' (List.mem [[v2]]) in
						   let forest_added = (List.append xs ys) :: forest' in
						   let forest_removed = List.filter ~f:(fun x -> x <> xs || x <> ys) forest_added in
						   Heap.remove_top edges';
						   while_loop forest_removed edges' (links' + 1)))
      in
    while_loop forest edges links
    in
    spanning forest edges
end

(*TEST CODE*)
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

 module FloatMatrix = ArrayMatrix(FloatCompare)

module IntMatrix = ArrayMatrix(IntCompare)

module FloatKruskal = Kruskal(FloatMatrix)

module IntKruskal = Kruskal(IntMatrix)

let test = IntKruskal.cluster (Kruskal 2) (IntMatrix.of_list [[0;1;0];
								 [1;0;100];
								 [0;100;0]])

let print_lists lsts =
  let print_list =
    List.iter ~f:(fun e -> print_int e; print_string " ")
  in List.iter ~f:(fun lst -> print_list lst; print_string "\n") lsts

let _ = print_lists test

 *)
