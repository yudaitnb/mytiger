{
  type token = [%import: Parser.token] [@@deriving show, eq]
  exception Error of string

  let n_of_nests = ref 0

  let gen_error_message (buf : Lexing.lexbuf) (proc : string) (error : string) =
    let pos = buf.lex_curr_p in
    let lnum = pos.pos_lnum in
    let start = buf.lex_start_pos in
    let curr = buf.lex_curr_pos in
    let last_char =
      (* 仮の条件式（lexbuf.lex_eof_reachedが機能しない） *)
      if (start == curr) then
        "eof"
      else (Lexing.lexeme buf)
    in
    let message = Printf.sprintf
      "line %d, characters %d-%d '%s': %s - %s"
      (lnum)          (* 処理に失敗した行番号 *)
      (start)         (* 処理に失敗した文字の開始位置 *)
      (curr)          (* 処理に失敗した文字位置 *)
      (last_char)     (* 処理に失敗した文字 *)
      (proc)          (* 処理に失敗した処理名 *)
      (error)         (* 処理の失敗の理由 *)
    in
      message
  let gen_error_message_lexer (buf : Lexing.lexbuf) (error : string)
    = gen_error_message buf "Lexer" error
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let number = digit+
let ident = ['a'-'z' 'A'-'Z'] (alpha | digit | '_')*

(* コメント *)
rule comment = parse
  (* 開き括弧の度にネスト数を一つ上げ、comment ruleでパース *)
  | "/*"
    { n_of_nests := !n_of_nests + 1;
      comment lexbuf }
  (* 閉じ括弧の度にネスト数を一つ下げ、0になったらcomment ruleを離脱 *)
  | "*/"
    { n_of_nests := !n_of_nests - 1;
      if !n_of_nests = 0 then
        token lexbuf
      else
        comment lexbuf }
  | '/' { comment lexbuf }
  | '*' { comment lexbuf }
  | [^ '/' '*'] { comment lexbuf }
  | eof { raise (Error (gen_error_message_lexer lexbuf "expected '*/', but not found")) }

and token = parse
  (* スペース *)
  | space+        { token lexbuf }
  
  (* コメント *)
  | "/*"
    { n_of_nests := !n_of_nests + 1;
      comment lexbuf }

  (* キーワード *)
  | "while"       { WHILE }
  | "for"         { FOR }
  | "to"          { TO }
  | "break"       { BREAK }
  | "let"         { LET }
  | "in"          { IN }
  | "end"         { END }
  | "function"    { FUNCTION }
  | "var"         { VAR }
  | "type"        { TYPE }
  | "array"       { ARRAY }
  | "if"          { IF }
  | "then"        { THEN }
  | "else"        { ELSE }
  | "do"          { DO }
  | "of"          { OF }
  | "nil"         { NIL }
  | ":="          { ASSIGN } 

  (* セパレータ *)
  | ","           { COMMA }
  | ":"           { COLON }
  | ";"           { SEMICOLON }
  | "."           { DOT }
  (* | "@"           { AT } *)

  (* 括弧 *)
  | "("           { LPAREN }
  | ")"           { RPAREN }
  | "["           { LBRACKET }
  | "]"           { RBRACKET }
  | "{"           { LBRACE }
  | "}"           { RBRACE }

  (* 演算子 *)
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "*"           { ASTERISK }
  | "/"           { SLASH }
  | "="           { EQ }
  | "<>"          { NEQ }
  | "<"           { LT }
  | "<="          { LTE }
  | ">"           { GT }
  | ">="          { GTE }
  | "&"           { AND }
  | "|"           { OR }

  (* 空リスト *)
  | "nil"         { NIL }

  (* 数字 *)
  | number as n   { INT (int_of_string n ) }

  (* 変数 *)
  | ident as id   { ID id }

  (* 文字列 *)
  | "\"" _* "\"" as str
    { let s = String.sub str 1 ((String.length str) - 2) in
      STR(s) }

  (* EOF *)
  | eof           { EOF }

  (* 不正文字 *)
  | _
    {
      raise (Error (gen_error_message_lexer lexbuf "illegal character"))
    }