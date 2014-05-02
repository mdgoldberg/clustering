(* The program to be run from the command line *)
open Core.Std
open Str
open Signatures
open Matrix
open Markov
open Kruskal
open Cartesian_graph
open Stats

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
module IntStats = Stats(IntMatrix)
module FloatStats = Stats(FloatMatrix)

let usage = ("Usage:\n./main.native markov|kruskal csvfile float|int " ^
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

let header : bool =
  Array.exists Sys.argv ~f:(fun arg -> arg = "--has-header")

let verbose : bool =
  Array.exists Sys.argv ~f:(fun arg -> arg = "--verbose")

let lines : string list = let raw_lines = In_channel.read_lines filename in
			  match raw_lines with
			  | _ :: tl -> if header then tl else raw_lines
			  | [] -> failwith "empty file"

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
    | "kruskal" -> (List.length lines) / 2
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
    | "markov" -> 3.
    | "kruskal" -> 0. (* argument doesn't matter *)
    | _ -> failwith "already checked, so not possible"

let delim : char =
  let arr =
    Array.filter Sys.argv~f:(fun arg -> String.length arg >= 8 &&
      String.slice arg 0 8 = "--delim=")
  in 
  try
    let s = arr.(0) in
    let delim_str = String.drop_prefix s 8 in
    Char.of_string delim_str
  with _ ->
    match alg_type with
    | _ -> ','

let process_file_float () : FloatMatrix.t =
  let process_line (line : string) : float list = 
    let raw_charnums = String.split (String.strip line) ~on:delim in
    let charnums = match raw_charnums with
      | _ :: tl -> if labels then tl else raw_charnums
      | [] -> failwith "Error: empty line"
    in 
    List.map charnums ~f:Float.of_string
  in
  if cartesian then
    FloatToGraph.to_graph (List.map lines ~f:process_line)
  else 
    FloatMatrix.of_list (List.map lines ~f:process_line)

let process_file_int () : IntMatrix.t =
  let process_line (line : string) : float list = 
    let raw_charnums = String.split (String.strip line) ~on:delim in
    let charnums = match raw_charnums with
      | _ :: tl -> if labels then tl else raw_charnums
      | [] -> failwith "Error: empty line"
    in 
    List.map charnums ~f:Float.of_string
  in
  let lsts = List.map lines ~f:process_line in
  if cartesian then
    IntToGraph.to_graph lsts
  else
    IntMatrix.of_list (List.map lsts
			 ~f:(fun lst -> List.map lst ~f:Int.of_float))

let get_labels : string array =
  let lines_arr = List.to_array lines in
  if labels then
    let proc_line line =
      let comma_ind = String.index_exn line delim in
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

let rec print_float_opts (lst : float option list) : unit =
  match lst with
  | hd :: tl ->
    begin
      match hd with
      | Some x -> print_float x; print_string "  "
      | None -> print_string "None  "
    end ;
    print_float_opts tl
  | [] -> print_string "\n"

let _ = match matrix_type, alg_type with
  | "int", "markov" ->
    let mat = process_file_int () in
    let clusts = IntMarkov.cluster (Markov (arg1,arg2,verbose)) mat in
    print_string "Clustering:\n";
    print_clusters clusts;
    print_string "Density (lower numbers -> more dense, None is bad):\n";
    let dense_vals = IntStats.avg_dist_all mat clusts in
    print_float_opts dense_vals;
    print_string "Spread (higher numbers -> more spread, None is good):\n";
    let spread_vals = IntStats.dist_between_all mat clusts in
    if List.length spread_vals > 0 then
      print_float_opts spread_vals
    else print_string "Only 1 cluster.\n"
  | "float", "markov" ->
    let mat = process_file_float () in
    let clusts = FloatMarkov.cluster (Markov (arg1,arg2,verbose)) mat in
    print_string "Clustering:\n";
    print_clusters clusts;
    print_string "Density (lower numbers -> more dense, None is bad):\n";
    let dense_vals = FloatStats.avg_dist_all mat clusts in
    print_float_opts dense_vals;
    print_string "Spread (higher numbers -> more spread, None is good):\n";
    let spread_vals = FloatStats.dist_between_all mat clusts in
    if List.length spread_vals > 0 then
      print_float_opts spread_vals
    else print_string "Only 1 cluster.\n"
  | "int", "kruskal" ->
    let mat = process_file_int () in
    let clusts = IntKruskal.cluster (Kruskal arg1) mat in
    print_string "Clustering:\n";
    print_clusters clusts;
    print_string "Density (lower numbers -> more dense, None is bad):\n";
    let dense_vals = IntStats.avg_dist_all mat clusts in
    print_float_opts dense_vals;
    print_string "Spread (higher numbers -> more spread, None is good):\n";
    let spread_vals = IntStats.dist_between_all mat clusts in
    if List.length spread_vals > 0 then
      print_float_opts spread_vals
    else print_string "Only 1 cluster.\n"
  | "float", "kruskal" ->
    let mat = process_file_float () in
    let clusts = FloatKruskal.cluster (Kruskal arg1) mat in
    print_string "Clustering:\n";
    print_clusters clusts;
    print_string "Density (lower numbers -> more dense, None is bad):\n";
    let dense_vals = FloatStats.avg_dist_all mat clusts in
    print_float_opts dense_vals;
    print_string "Spread (higher numbers -> more spread, None is good):\n";
    let spread_vals = FloatStats.dist_between_all mat clusts in
    if List.length spread_vals > 0 then
      print_float_opts spread_vals
    else print_string "Only 1 cluster.\n"
  | _ -> invalid_arg "no clustering algorithm found"

