{
  type token = [%import: Parser.token] [@@deriving show, eq]
  exception SyntaxError of string
}

let digit = ['0'-'9']
let number = digit+
let whitespace = [' ' '\t' '\n' '\r']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' 'A'-'Z']+ (alpha | digit | '_')*

rule tokenize = parse
  | whitespace+   { tokenize lexbuf }
  (* | "while"       { WHILE }
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
  | ","           { COMMA }
  | ":"           { COLON }
  | ";"           { SEMICOLON }
  | "("           { LPAREN }
  | ")"           { RPAREN }
  | "["           { LBRACKET }
  | "]"           { RBRACKET }
  | "{"           { LBRACE }
  | "}"           { RBRACE }
  | "."           { DOT } *)
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "*"           { TIMES }
  | "/"           { DIV }
  | "<>"          { NEQ }
  | "<"           { LT }
  | "<="          { LTE }
  | ">"           { GT }
  | ">="          { GTE }
  | "&"           { AND }
  | "|"           { OR }
  | ":="          { ASSIGN } 
  | number as n   { INT (int_of_string n ) }
  (* | ident as id   { ID id }
  | "\"" _* "\"" as str { let s = String.sub str 1 ((String.length str) - 2) in STR(s) }
  | "/*" _* "*/"        { tokenize lexbuf } 
  | _* as str { STR(str) } *)
  | _             { raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { EOF }
  (* | eof { raise (SyntaxError ("String is not terminated")) } *)