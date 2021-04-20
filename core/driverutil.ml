open Lexer
open Parser
open Printf
open Semantic
open Ast
open Types

(* string値を受け取って出力する *)
let printl (str : string) = printf "%s\n" str

(* Lexing bufferを受け取り、Lexing後に生成されるtoken listを生成 *)
let rec tokens_of buf =
  match token buf with
  | EOF -> [ Parser.EOF ]
  | tok -> tok :: tokens_of buf

(* token listをcomma separatedな文字列に変換 *)
let show_list_of_tokens (toks : Parser.token list) =
  String.concat ", " (List.map Lexer.show_token toks)

(* Lexing bufferを受け取り、Lexing後に生成されるtoken listを標準出力に表示 *)
let print_tokens (buf : Lexing.lexbuf) =
  try
    printf "Lexing result: %s\n" (show_list_of_tokens (tokens_of buf))
  with
  | Lexer.Error error_msg -> printl error_msg

(* 
 * tokens_ofの引数str版
 * Lexing後に生成されるtoken listを生成
 *)
let tokens_of_from_string str =
  let lexbuf = Lexing.from_string str in
  tokens_of lexbuf

(* 
 * parse_programの引数lexbuf版
 *)
let parse_program lexbuf =
  printl "* Parsing ...";
  let res_parser = Parser.prog Lexer.token lexbuf in
  res_parser

(* 
 * parse_programの引数str版
 *)
 let parse_from_string str =
  let lexbuf = Lexing.from_string str in
  parse_program lexbuf

(* 
 * astの出力用
 *)
let print_ast ast =
  printf "==== AST ===\n  %s\n" (show_exp ast)

(* 
 * Ast -> type
 *)
let type_from_ast ast =
  printl "* Type checking ...";
  let res_typechecker = typeof ast in
  res_typechecker

(* 
 * tyの出力用
 *)
let print_type ty =
  printf "=== Type ===\n  %s\n" (show_ty ty)
