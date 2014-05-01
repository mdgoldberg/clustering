(* The program to be run from the command line *)
open Core.Std

(*

usage:
./main.native markov|kruskal [arg1] [arg2] [--cartesian] csvfile

(order doesn't matter)

*)

let process_file (filename : string) : float list list =
  let process_line (line : string) : float list = 
    let charnums = String.split (String.strip line) ~on:(',') in
    List.map charnums ~f:Float.of_string
  in
  let lines = In_channel.read_lines filename in
  List.map lines ~f:process_line

let filename : string =
  let arr = Array.filter Sys.argv
    ~f:(fun arg ->
      let newarg = String.strip arg in
      let len = String.length newarg in
      String.slice newarg (len-4) len = ".csv")
  in
  try
    arr.(0)
  with _ ->
    print_string ("\nUsage:\n./main.native markov|kruskal csvfile " ^
      "[arg1] [arg2] [--cartesian]\norder doesn't matter.\n");
    raise Not_found

let cartesian : bool =
  Array.exists Sys.argv ~f:(fun arg -> arg = "--cartesian")
  
let _ = if cartesian then
    print_string "cartesian" else
    print_string "not cartesian" in
print_string "\n"
