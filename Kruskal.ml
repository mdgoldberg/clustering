open Core.Std
open Matrix
open Signatures

module Kruskal = 
struct

	let loop (m : M.t) : ('a Heap.t) =
		let q = Heap.create ~cmp:compare () in
		let (a, b) = M.dimensions m in
		let size = max a b in
		let _ =
		for i = 0 to size do
			for j = 0 to size do 
				Heap.add q (M.get (i, j) m, i, j)
			done
		done 
		in
		q

	(*let rec make_forest (m: M.t) (size : int): Int_set.t= 
		if size = 0
		then Int_set.empty
		else Int_set.add(size) *)

	let is_spanning (trees: 'a Bag.t) (size: int) : bool =
		let rec sizes (n : int) =
			match n with
			| 0 -> []
			| x -> n :: (sizes x) in
		(Bag.fold ~f: (+) ~init: 0 trees) = (List.fold_left ~f: (+) ~init: 0 (sizes size))
		

	

	let remove_edges (tree: 'a list) (n: int) : 'a list list =
		[[]]

	let cluster (n: int) (m : ArrayMatrix(Matrix.FloatCompare).t) : 'a list list =
		(*edges = loop m*)
		(*size = m.dimension*)
		(*forest = make_forest m size*)
		(*tree = spanning forest edges*)
		(*remove_edges tree n*)
		let spanning (forest: 'a Bag.t) (edges: 'a Heap.t) : 'a list = 
			while not (Heap.is_empty edges) && not (is_spanning forest n) 
			do
				(match Heap.top edges with
				| None -> ()
				| Some e -> ());
				Heap.remove_top edges;
				(*Check if the edge will connect two trees*)
				(*Connect two trees*)
			done;
			[]
		in
		[[]]
end