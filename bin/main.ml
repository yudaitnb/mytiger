open Core

let rec pprint = function
  | Ast.Int n -> string_of_int n
  | Ast.Plus (n, m) -> Printf.sprintf "(Plus %s %s)" (pprint n) (pprint m)

let rec eval = function
  | Ast.Int n -> n
  | Ast.Plus (n, m) -> eval n + eval m

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.prog Lexer.tokenize lexbuf in
  Printf.printf "%s = %d\n" (pprint expr) (eval expr)