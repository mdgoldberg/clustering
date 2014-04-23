open Core
open Signatures

module type MATRIX = 
sig

  (*type of the elements*)
  type elt

  (*implementation of the matrix itself*)
  type t

  exception Invalid_Dimensions

  (*creates a matrix out of list of lists*)
  val of_list : elt list list -> t
  
  (*multiplies two matrices together*)
  val multiply : t -> t -> t

  (*returns an element of the matrix *)
  val get : (int * int) -> t -> elt

  (*returns the dimensions of a matrix*)
  val dimensions : t -> (int * int)

end

(* A module signature for comparing data types *)
module type COMPARABLE =
sig 
  
  (* Type *)
  type t
  
  (* Default of type, for array creation *)
  val default : t

  (* Comparison function *)
  val compare : t -> t -> Ordering.t

end

(* For testing: *)
module IntCompare : COMPARABLE  with type t = int =
struct 
  type t = int
  let default = 0
  let compare a b = Ordering.of_int (a - b)
end

(* Array implementation *)
module ArrayMatrix (C : COMPARABLE) : MATRIX with type elt = C.t =
struct

	type elt = C.t

	type t = elt array array

	exception Invalid_Dimensions

	let of_list (lst : elt list list) : t = 
	  let rec loop lst ret i =
	    match lst with
	    | [] -> ret
	    | x :: xs ->
	      ret.(i) <- Array.of_list x;
	      loop xs ret (i+1)
	  in loop lst (Array.create (List.length lst) [| C.default |]) 0

	let dimensions (m: t) : (int * int) =
	  (Array.length m, 0)

	let multiply (m1 : t) (m2 : t) : t = 
		if (dimensions m1) <> (dimensions m2)
		then raise Invalid_Dimensions
		else failwith "not implemented"

	let get ((x, y) : (int * int)) (m: t) = 
		Array.get (Array.get m x) y

end
