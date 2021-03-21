%{
open Exp
%}

%token WHILE
%token FOR
%token TO
%token BREAK
%token LET
%token IN
%token END
%token FUNCTION
%token VAR
%token TYPE
%token ARRAY
%token IF
%token THEN
%token ELSE
%token DO
%token OF
%token NIL

%token COMMA
%token COLON
%token SEMICOLON
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token DOT

%token PLUS
%token MINUS
%token TIMES
%token DIV

%token EQ
%token NEQ
%token LT
%token LTE
%token GT
%token GTE
%token AND
%token OR
%token ASSIGN
%token EOF

// %token <string> ID
%token <int> INT
%token <string> STR

%left PLUS MINUS
%left TIMES DIV

%start <exp> prog

%%

prog:
  | e = exp EOF { e }

exp:
  | LPAREN exp RPAREN { $2 }
  | INT { Int $1 }
  | exp PLUS exp { Add ($1, $3) }
  | exp MINUS exp { Sub ($1, $3) }
  | exp TIMES exp { Mul ($1, $3) }
  | exp DIV exp { Div ($1, $3) }