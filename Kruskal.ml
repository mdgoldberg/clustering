open Core
open matrix.ml
open signatures

module Kruskal = struct

	let loop (m : MATRIX.t) : PriorityQueue =
		let q = PriorityQueue.make Comparable.compare in
		let (a, b) = m.dimension in
		let size = max a b in
		for i = 0 to size do
			for j = 0 to size do 
				q.add (m.get (i, j), i, j)

	let rec make_forest (m: MATRIX.t) (size : int): Set.t= 
		if size = 0
		then Set.empty
		else Set.union (Set.singleton [size])

	let is_spanning (trees: Set.t) (size: int) : bool =
		let check = (List.fold_left ~f: (+) ~init: 0 tree) = (List.fold_left ~f: (+) ~init: 0 size) in
		Set.iter check trees

	let spanning (forest: Set.t) (edges: PriorityQueue) : 'a list = 
		while not (PriorityQueue.empty edges) && not (is_spanning forest) do
			let (e, v1, v2)= PriorityQueue.first edges in
			PriorityQueue.remove_first edges;
			
			done


	let cluster (n: int) (m : MATRIX.t) : a' list list =
		[[]]
end