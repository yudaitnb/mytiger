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

  (* 文字列lexing用の関数 *)
  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char
  let unterminated_string loc =
    Error.error loc "unterminated string"
  let illegal_escape loc sequence =
    Error.error loc "illegal escape sequence: %s" sequence
  let append_char str ch =
    str ^ (String.make 1 (Char.chr ch))
  let str_incr_linenum str lexbuf =
    String.iter (function '\n' -> Lexing.new_line lexbuf | _ -> ()) str
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
  | '"'           { string lexbuf.Lexing.lex_start_p "" lexbuf }

  (* EOF *)
  | eof           { EOF }

  (* 不正文字 *)
  | _
    {
      raise (Error (gen_error_message_lexer lexbuf "illegal character"))
    }

and string pos buf = parse
  (* 閉じダブルクオートに到達したらbufferをSTRでラップして返す *)
  | '"'                               { lexbuf.Lexing.lex_start_p <- pos;
                                        STR buf
                                      }

  (* 行の終りとしてシステムに解釈される文字 *)
  | "\\n"                             { string pos (buf ^ "\n") lexbuf }

  (* タブ *)
  | "\\t"                             { string pos (buf ^ "\t") lexbuf }

  (* 全ての適切なcに対応する制御文字c *)
  | "\\^" (['@' 'A'-'Z'] as x)        { string pos (append_char buf (Char.code x - Char.code '@')) lexbuf }
  | "\\^" (['a'-'z'] as x)            { string pos (append_char buf (Char.code x - Char.code 'a' + 1)) lexbuf }

  (* ASCIIコードdddを持つ一つの数字 *)
  | "\\" (digit digit digit as x)     { string pos (append_char buf (int_of_string x)) lexbuf }

  (* ダブルクオート文字 ('"') *)
  | "\\\""                            { string pos (buf ^ "\"") lexbuf }

  (*
   * バックスラッシュ文字 ('\')
   * (ocamlの対応するエスケープシーケンスが'\\'の為,4つ必要)
   *)
  | "\\\\"                            { string pos (buf ^ "\\") lexbuf }

  (* 行跨ぎの文字列 *)
  | "\\" ([' ' '\t' '\n']+ as x) "\\" { str_incr_linenum x lexbuf;
                                        string pos buf lexbuf
                                      }

  (* 不正なエスケープシーケンス *)
  | "\\" _ as x                       { illegal_escape (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p) x;
                                        string pos buf lexbuf
                                      }
  
  (*  *)
  | [^ '\\' '"']+ as x                { str_incr_linenum x lexbuf;
                                        string pos (buf ^ x) lexbuf
                                      }

  (* 閉じダブルクオートが無い文字列 *)
  | eof                               { unterminated_string (pos, lexbuf.Lexing.lex_start_p);
                                        token lexbuf
                                      }