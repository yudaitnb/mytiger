open Core
open Printf
open Util

let path_queens = "./examples/examplesfrombook/testcases/queens.tig"
let path_merge = "./examples/examplesfrombook/testcases/merge.tig"

let () =
  let input = Lexing.from_channel stdin in
  match parse_program input with
  | Error msg -> print_endline msg
  | Ok res    -> printf "   AST: \n%s\n" (Ast.show_exp res)

(* let file_to_string path =
  let fin = open_in path in
  let buff = ref "" in
  let rec file_to_string_sub () =
    let xs = input_line fin in
    buff := (!buff) ^ xs;
    file_to_string_sub ()
  in
  try file_to_string_sub () with
    End_of_file ->
      close_in fin;
      !buff

let () =
  let input = file_to_string path_queens in
  let res = parse_from_string input in
  printf "==== INPUT: ====\n%s\n" input;
  printf "==== AST: ====\n%s\n" (Ast.show_exp res) *)
