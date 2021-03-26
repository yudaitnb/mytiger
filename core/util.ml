open Lexer
open Parser
open Printf
open Error

(* string値を受け取って出力する *)
let printl (str : string) = printf "%s\n" str

(* let show_tokens_from_string str =
  let lexbuf = Lexing.from_string str in
  show_tokens_from_lexbuf lexbuf

let print_tokens_from_string (str : string) =
  try
    printf "Result: %s\n" (show_token_list (show_tokens_from_string str))
  with
  | Lexer.Error error_msg -> printl error_msg *)

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

(* let tokenize (buf : Lexing.lexbuf) =
  try Ok (Lexer.token buf) with
  | Lexer.Error msg -> printl msg; Error msg *)

let parse_program lexbuf =
  printl "parsing ...\n";
  try Ok (Parser.prog Lexer.token lexbuf) with
  | Lexer.Error msg ->
      let error_msg = msg in
      Error (error_msg) (* Lexer.mllでエラー生成 *)
  | Parser.Error ->
      let error_msg = gen_error_message lexbuf "Parser" "Unexpected term" in
      Error (error_msg)
