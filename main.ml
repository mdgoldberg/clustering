(* The program to be run from the command line *)
open Core.Std
open Str
open Signatures
open Matrix
open Markov
open Cartesian_graph

(*

usage:
./main.native markov|kruskal [--arg1=A] [--arg2=B] [--cartesian] csvfile float|int

(order doesn't matter)

*)

module IntCompare : COMPARABLE  with type t = int =
struct 
  type t = int
  let zero = 0
  let compare a b = Ordering.of_int (a - b)
  let multiply a b = a * b
  let add a b = a + b
  let print t = print_int t
  let float_of_t t = Float.of_int t
  let t_of_float f = Int.of_float f
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

module IntMatrix = ArrayMatrix(IntCompare)
module FloatMatrix = ArrayMatrix(FloatCompare)
module IntToGraph = Cartesian(IntMatrix)
module FloatToGraph = Cartesian(FloatMatrix)
module IntMarkov = Markov(IntMatrix)
module FloatMarkov = Markov(FloatMatrix)

let usage = ("\nUsage:\n./main.native markov|kruskal csvfile float|int " ^
      "[--arg1=A] [--arg2=B] [--cartesian]\norder doesn't matter.\n")

let filename : string =
  let arr = Array.filter Sys.argv
    ~f:(fun arg ->
      let len = String.length arg in
      String.slice arg (len-4) len = ".csv")
  in
  try
    arr.(0)
  with _ ->
    print_string usage;
    invalid_arg "FILE csvfile NOT FOUND"

let cartesian : bool =
  Array.exists Sys.argv ~f:(fun arg -> arg = "--cartesian")

let matrix_type : string =
  let arr =
    Array.filter Sys.argv ~f:(fun arg -> arg = "float" || arg = "int")
  in
  try
    arr.(0)
  with _ ->
    print_string usage;
    invalid_arg "MATRIX TYPE float|int NOT FOUND"

let alg_type : string =
  let arr = 
    Array.filter Sys.argv ~f:(fun arg -> arg = "kruskal" || arg = "markov")
  in 
  try
    arr.(0)
  with _ ->
    print_string usage;
    invalid_arg "ALGORITHM TYPE kruskal|markov NOT FOUND"

let arg1 : int =
  let arr =
    Array.filter Sys.argv~f:(fun arg -> String.length arg >= 7 &&
      String.slice arg 0 7 = "--arg1=")
  in 
  try
    let s = arr.(0) in
    let first_ind = (String.index_exn s '=') + 1 in
    let strnum = String.slice s first_ind (String.length s) in
    Int.of_string strnum
  with _ ->
    match alg_type with
    | "markov" -> 2
    | "kruskal" -> 0 (* Kruskal default *)
    | _ -> failwith "already checked, so not possible"

let arg2 : float =
  let arr =
    Array.filter Sys.argv~f:(fun arg -> String.length arg >= 7 &&
      String.slice arg 0 7 = "--arg2=")
  in 
  try
    let s = arr.(0) in
    let first_ind = (String.index_exn s '=') + 1 in
    let strnum = String.slice s first_ind (String.length s) in
    Float.of_string strnum
  with _ ->
    match alg_type with
    | "markov" -> 2.
    | "kruskal" -> 0. (* argument doesn't matter *)
    | _ -> failwith "already checked, so not possible"


let process_file_float (filename : string) : FloatMatrix.t =
  let process_line (line : string) : float list = 
    let charnums = String.split (String.strip line) ~on:(',') in
    List.map charnums ~f:Float.of_string
  in
  let lines = In_channel.read_lines filename in
  if cartesian then
    FloatToGraph.to_graph (List.map lines ~f:process_line)
  else 
    FloatMatrix.of_list (List.map lines ~f:process_line)

let process_file_int (filename : string) : IntMatrix.t =
  let process_line (line : string) : float list = 
    let charnums = String.split (String.strip line) ~on:(',') in
    List.map charnums ~f:Float.of_string
  in
  let lines = In_channel.read_lines filename in
  let lsts = List.map lines ~f:process_line in
  if cartesian then
    IntToGraph.to_graph lsts
  else
    IntMatrix.of_list (List.map lsts
			 ~f:(fun lst -> List.map lst ~f:Int.of_float))

let print_clusters (lsts : int list list) : unit =
  let print_list =
    List.iter ~f:(fun e -> print_int e; print_string " ")
  in List.iter ~f:(fun lst -> print_list lst; print_string "\n") lsts

(*
  let module ClusterMod = match matrix_type (), alg_type () with
  | "int", "markov" -> IntMarkov
  | "float", "markov" -> FloatMarkov
  | "int", "kruskal" -> (* IntKruskal *) failwith "not done yet"
  | "float", "kruskal" -> (* FloatKruskal *) failwith "not done yet"
  | _ -> invalid_arg "no clustering algorithm found"
  in 
*)
let _ = print_clusters (IntMarkov.cluster (Markov (arg1,arg2))
			  (process_file_int filename))
