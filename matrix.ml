open Core
open signatures

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
 
  (*returns the smallest non-zero element of a matrix*)
  val minimum : t -> elt

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

  (* Multiplication function*)
  val multiply : t -> t -> t

  (* Addition *)
  val add : t -> t -> t

end

(* For testing: *)
module IntCompare : COMPARABLE  with type t = int =
struct 
  type t = int
  let default = 0
  let compare a b = Ordering.of_int (a - b)
  let multiply a b = a * b
  let add a b = a + b
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
    (Array.length m, Array.length m.(0))

  let multiply (m1 : t) (m2 : t) : t = 
   (* let (r1, c1) = dimensions m1 in
    let (r2, c2) = dimensions m2 in
    if c1 <> r2
    then raise Invalid_Dimensions
    else 
      let res = Array.make_matrix r1 c2 C.t
      for i = 0 to max_columns do 
	for j = 0 to max_rows do
	  res.(i).(j) <- res.(i).(j) C.add (C.multiply a.(i).(j) b.(j).(i)) *)
    failwith "asda"
	  

  let get ((x, y) : (int * int)) (m: t) = 
    Array.get (Array.get m x) y

  let minimum (m : t) : elt =
    failwith "not implemented"

end

module IntMatrix = ArrayMatrix(IntCompare)

let t = IntMatrix.of_list [[1;2];[3;4]]
