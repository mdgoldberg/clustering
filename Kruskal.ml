open Core.Std
open Matrix
open Signatures

module Kruskal = 
struct


	let make_edgelist (m : M.t) : ('a Heap.t) =
		let q = Heap.create ~cmp:compare () in
		let (a, b) = M.dimensions m in
		let size = max a b in
		let _ =
		for i = 0 to size - 1 do
			for j = 0 to size - 1 do 
				if M.get (i, j) = 0
				then ()
				else Heap.add q (M.get (i, j) m, i, j)
			done
		done 
		in
		q

	let make_forest (m: M.t) (size : int): Bag.t= 
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
			while not (Heap.is_empty edges) && not (is_spanning forest (size - 1) && (links < max_links)
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
														  links++;
				Heap.remove_top edges;
				(*Check if the edge will connect two trees*)
				(*Connect two trees*)
			done;
			[Bag.to_list forest]
		in
		(*remove_edges tree n*)
		spanning forest edges
end