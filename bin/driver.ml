open Core
open Printf
open Util


let path = "../exampels/examplesfrombook/testcases/merge.tig"

let file_to_string path =
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
      print_endline !buff;
      !buff

let () =
  let input = file_to_string path in
  match parse_from_string input with
  | Error msg -> print_endline msg
  | Ok res    -> printf "   AST: \n%s\n" (Ast.show_exp res)