open Core

module type MATRIX = 
sig

  (*type of the elements*)
  type elt

  (*implementation of the matrix itself*)
  type t

  exception invalid_dimensions

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

module MATRIX =
struct

  type elt = float

  type t = elt array array

  exception invalid_dimensions

  let of_list (lst : elt list list) : t = 
    let rec loop  =
      match lst with
      | [] -> [||]
      | x :: xs -> Array.append (Array.of_list x) loop xs

  let dimensions (m: t) : (int * int) =
		

  let multiply (m1 : t) (m2 : t) : t = 
    if (dimensions m1) <> (dimensions m2)
    then raise invalid_dimensions
    else 

  let get ((x, y) : (int * int)) (m: t) = 
    Array.get (Array.get m x) y

end
