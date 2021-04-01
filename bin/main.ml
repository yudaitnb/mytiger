open Core
open Printf
open Util

(* 
let ex1 = "./mytiger/examples/ex1"

let parse_file path = 
  let ic = open_in path in
  try 
    let line = input_line ic in  (* in_channel から1行読んで捨てる \n *)
    print_endline line;          (* 結果を stdout に書く *)
    flush stdout;                (* ここで実際に対象デバイスに書き込む *)
    match parse_program (Lexing.from_string line) with
    | Error msg -> print_endline msg
    | Ok res    -> print_endline (sprintf "AST: %s" (Ast.show_exp res));
    close_in ic                  (* 入力チャネルを閉じる *)
  with e ->                      (* 期待しない例外が起こったとき *)
    close_in_noerr ic;           (* 緊急にチャネルを閉じる *)
    raise e                      (* エラー終了: ファイルは閉じられるが
                                    チャネルはフラッシュされない *) *)

let () =
  let input = Lexing.from_channel stdin in
  (* let expr = Parser.prog Lexer.token input in
  (* printf "result: %s\n" (Lexer.show_token (Lexer.token input)) *)
  (* printf "   AST: %s\n" (Ast.show_exp expr) *)
  print_endline (sprintf "   AST: %s\nresult: %d\n" (Ast.show_exp expr) (Evaluator.eval expr)) *)
  match parse_program input with
  | Error msg -> print_endline msg
  | Ok res    -> printf "   AST: \n%s\n" (Ast.show_exp res)
    (* printf "   AST: \n%s\n" (Ast.show_exp res.ast) *)