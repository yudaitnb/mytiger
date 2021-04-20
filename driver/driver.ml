open Core.Driverutil

let path_queens = "./examples/examplesfrombook/testcases/queens.tig"
let path_merge = "./examples/examplesfrombook/testcases/merge.tig"

let input_path = path_merge

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
      !buff

let () =
  let inchan = open_in input_path in
  let res_parser = parse_program (Lexing.from_channel inchan) in
  let res_typechecker = type_from_ast res_parser in
  print_ast res_parser;
  print_type res_typechecker;
