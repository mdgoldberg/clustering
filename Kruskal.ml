open Core.Std
open Matrix
open Signatures


module Kruskal : CLUSTER = 
	functor (M: MATRIX) ->
struct


	let make_edgelist (m : M.t) : ('a Heap.t) =
		let q = Heap.create ~cmp:compare () in
		let (a, b) = M.dimensions m in
		let size = max a b in
		let _ =
		for i = 0 to size - 1 do
			for j = 0 to size - 1 do 
				(*if (M.get (i, j) m) = 0
				then ()
				else*) Heap.add q (M.get (i, j) m, i, j)
			done
		done 
		in
		q

	let make_forest (m: M.t) (size : int): Bag.t = 
		let rec loop (s: Bag.t) (n : int) =
			if n < 0
			then s
			else loop (Bag.add s [size]) (n - 1)
		in
		loop (Bag.create ()) size

	let is_spanning (trees: 'a Bag.t) (size: int) : bool =
		let rec sizes (n : int) =
			match n with
			| 0 -> [0]
			| x -> n :: (sizes x) in
		(Bag.fold ~f: (+) ~init: 0 trees) = (List.fold_left ~f: (+) ~init: 0 (sizes size ))
		

	let remove_edges (tree: 'a list) (n: int) : 'a list list =
		[[]]

	let cluster (n: int) (m : ArrayMatrix(Matrix.FloatCompare).t) : 'a list list =
		let size = M.dimensions m in
		let edges = make_edgelist m in 
		let forest = make_forest m (size - 1) in
		let links = 0 in
		let max_links = size - n in
		let spanning (forest: 'a Bag.t) (edges: 'a Heap.t) : 'a list list = 
			while (not (Heap.is_empty edges) && not (is_spanning forest (size - 1)) && (links < max_links))
			do
				(match Heap.top edges with
				| None -> (*this should never happen*) ()
				| Some (e, v1, v2)-> (match Bag.find forest (List.mem v1) with
									 | None -> ()
									 | Some (xs) -> if (List.mem v2 xs) then (*a cycle would occur*)() 
													else let Some ys = Bag.find forest (List.mem v2) in
														  Bag.add (List.append ys xs);
														  Bag.remove xs;
														  Bag.remove ys;
														  links = links + 1;));
				Heap.remove_top edges;
				(*Check if the edge will connect two trees*)
				(*Connect two trees*)
			done;
			Bag.to_list forest
		in
		(*remove_edges tree n*)
		spanning forest edges
end


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

let test = IntKruskal.cluster (2 (IntMatrix.of_list [[0;1;0];
								 [1;0;100];
								 [0;100;0]]))

let print_lists lsts =
  let print_list =
    List.iter ~f:(fun e -> print_int e; print_string " ")
  in List.iter ~f:(fun lst -> print_list lst; print_string "\n") lsts

let _ = print_lists test