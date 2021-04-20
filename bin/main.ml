open Core.Driverutil

let () =
  let input = Lexing.from_channel stdin in
  let res_parser = parse_program input in
  let res_typechecker = type_from_ast res_parser in
  print_ast res_parser;
  print_type res_typechecker
