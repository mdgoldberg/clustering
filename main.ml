(* The program to be run from the command line *)
open Core.Std
open Str
open Signatures
open Matrix
open Markov
open Cartesian_graph

(*

usage:
./main.native markov|kruskal [--arg1=A] [--arg2=B] [--cartesian] [--has-labels] csvfile float|int

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
module IntKruskal = Kruskal(IntMatrix)
module FloatKruskal = Kruskal(FloatMatrix)

let usage = ("\nUsage:\n./main.native markov|kruskal csvfile float|int " ^
      "[--arg1=A] [--arg2=B] [--cartesian] [--has-labels]\n" ^
      "order doesn't matter.\n")

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

let labels : bool =
  Array.exists Sys.argv ~f:(fun arg -> arg = "--has-labels")

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
    let strnum = String.drop_prefix s 7 in
    Int.of_string strnum
  with _ ->
    match alg_type with
    | "markov" -> 2
    | "kruskal" -> 0 (* TODO Kruskal default *)
    | _ -> failwith "already checked, so not possible"

let arg2 : float =
  let arr =
    Array.filter Sys.argv~f:(fun arg -> String.length arg >= 7 &&
      String.slice arg 0 7 = "--arg2=")
  in 
  try
    let s = arr.(0) in
    let strnum = String.drop_prefix s 7 in
    Float.of_string strnum
  with _ ->
    match alg_type with
    | "markov" -> 2.
    | "kruskal" -> 0. (* argument doesn't matter *)
    | _ -> failwith "already checked, so not possible"


let process_file_float (filename : string) : FloatMatrix.t =
  let process_line (line : string) : float list = 
    let raw_charnums = String.split (String.strip line) ~on:(',') in
    let charnums = match raw_charnums with
      | _ :: tl -> if labels then tl else raw_charnums
      | [] -> failwith "Error: empty line"
    in 
    List.map charnums ~f:Float.of_string
  in
  let lines = In_channel.read_lines filename in
  if cartesian then
    FloatToGraph.to_graph (List.map lines ~f:process_line)
  else 
    FloatMatrix.of_list (List.map lines ~f:process_line)

let process_file_int (filename : string) : IntMatrix.t =
  let process_line (line : string) : float list = 
    let raw_charnums = String.split (String.strip line) ~on:(',') in
    let charnums = match raw_charnums with
      | _ :: tl -> if labels then tl else raw_charnums
      | [] -> failwith "Error: empty line"
    in 
    List.map charnums ~f:Float.of_string
  in
  let lines = In_channel.read_lines filename in
  let lsts = List.map lines ~f:process_line in
  if cartesian then
    IntToGraph.to_graph lsts
  else
    IntMatrix.of_list (List.map lsts
			 ~f:(fun lst -> List.map lst ~f:Int.of_float))

let get_labels : string array =
  let lines = In_channel.read_lines filename in
  let lines_arr = List.to_array lines in
  if labels then
    let proc_line line =
      let comma_ind = String.index_exn line ',' in
      let len = String.length line in
      String.drop_suffix line (len - comma_ind)
    in 
    Array.map lines_arr ~f:proc_line
  else 
    Array.mapi lines_arr ~f:(fun i _ -> Int.to_string i)
    

let print_clusters (lsts : int list list) : unit =
  let print_list =
    List.iter ~f:(fun e -> print_string (get_labels.(e)); print_string " ")
  in List.iter ~f:(fun lst -> print_list lst; print_string "\n") lsts


match matrix_type, alg_type with
  | "int", "markov" -> print_clusters
    (IntMarkov.cluster (Markov (arg1,arg2)) (process_file_int filename))
  | "float", "markov" -> print_clusters
    (FloatMarkov.cluster (Markov (arg1,arg2)) (process_file_float filename))
  | "int", "kruskal" -> print_clusters
    (IntKruskal.cluster (Kruskal arg1) (process_file_int filename))
  | "float", "kruskal" -> print_clusters
    (FloatKruskal.cluster (Kruskal arg1) (process_file_float filename))
  | _ -> invalid_arg "no clustering algorithm found"

