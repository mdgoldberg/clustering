open Core.Std
open Signatures


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

  (* Prints specific types *)
  val print : t -> unit

end

(* For testing: *)
module IntCompare : COMPARABLE  with type t = int =
struct 
  type t = int
  let default = 0
  let compare a b = Ordering.of_int (a - b)
  let multiply a b = a * b
  let add a b = a + b
  let print t = print_int t 
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
    let (r1, c1) = dimensions m1 in
    let (r2, c2) = dimensions m2 in
    if c1 <> r2
    then raise Invalid_Dimensions
    else 
      let res = Array.make_matrix r1 c2 C.default in
      for i = 0 to r1 - 1 do 
	for j = 0 to c2 - 1 do
	  for k = 0 to c1 - 1 do
	    res.(i).(j) <- C.add (C.multiply m1.(i).(k) m2.(k).(j)) res.(i).(j) 
	  done;
	done;
      done;
    res 							
    
	  

  let get ((x, y) : (int * int)) (m: t) = 
    Array.get (Array.get m x) y

  let minimum (m : t) : elt =
    failwith "unimplemented"

  let print (m: t) : unit = 
    let print_row (row : elt array) =
      Array.iter row C.print in
    Array.iter m print_row 
	 
end

module M = ArrayMatrix(IntCompare)


assert(M.multiply (M.of_list [[1;2];[3;4]]) (M.of_list [[1;0];[0;1]]) 
       = (M.of_list [[1;2];[3;4]]));;
assert(M.multiply (M.of_list [[1;2];[3;4]]) (M.of_list [[0;0];[0;0]]) 
       = (M.of_list [[0;0];[0;0]]));;
assert(M.multiply (M.of_list [[1;2];[3;4]]) (M.of_list [[5;6];[7;8]]) 
       = (M.of_list [[19;22];[43;50]]));;
assert(M.multiply (M.of_list [[1;2]]) (M.of_list [[5];[8]]) 
       = (M.of_list [[21]]));;
assert(M.multiply (M.of_list [[1;2;3;4];[5;6;7;8]]) 
		  (M.of_list [[9;10];[11;12];[13;14];[15;16]]) 
       = (M.of_list [[130;140];[322;348]]));;

let t = M.of_list [[9;10];[11;12];[13;14];[15;16]] in
M.print t


print_int 4

