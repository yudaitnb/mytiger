%{
open Exp
%}

// キーワード
%token WHILE DO
%token FOR TO
%token BREAK
%token IF THEN ELSE
%token LET IN END
%token TYPE
%token FUNCTION
%token VAR ASSIGN
%token ARRAY OF
%token NIL

// セパレータ
%token COMMA COLON SEMICOLON DOT

// 括弧
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE

// 演算子
%token PLUS MINUS ASTERISK SLASH
%token EQ NEQ LT LTE GT GTE
%token AND OR

// リテラル
%token <string> ID  // 変数
%token <int> INT    // 数字
%token <string> STR // 文字列
%token <bool> BOOL // 文字列

// EOF
%token EOF

// 演算子の結合順位
%left EQ NEQ GT GTE LT LTE 
%left APP
%left PLUS MINUS
%left TIMES DIV
%nonassoc UNARY

%start <exp> prog

%%

// プログラムは大きな一つの式
prog:
  | exp EOF { $1 }

// 式
exp:
  | LPAREN exp RPAREN { $2 }

  // リテラル
  | INT               { Lit (Int $1) }
  | STR               { Lit (Str $1) }
  | BOOL              { Lit (Bool $1) }

  // レコード
  // nil
  // type_id {v1=e1, .. ,vn=2n}
  // var.label
  | NIL
    { Nil }
  | type_id=ID LBRACE fields=separated_list(COMMA, field) RBRACE
    { RecordExp { record_fields=fields; record_type=type_id } }
  | arr=exp DOT label=ID
    { DotExp {record=arr; label=label} }

  // 配列
  // type-id [e1] of e2
  | type_id=ID LBRACKET e1=exp RBRACKET OF e2=exp
    { ArrayExp { size=e1; init=e2; array_type=type_id } }

  // 変数
  | ID                { Var $1 }

  // 二項演算
  | exp PLUS exp      { BinOp { op=Add; e1=$1; e2=$3 } }
  | exp MINUS exp     { BinOp { op=Sub; e1=$1; e2=$3 } }
  | exp ASTERISK exp  { BinOp { op=Mul; e1=$1; e2=$3 } }
  | exp SLASH exp     { BinOp { op=Div; e1=$1; e2=$3 } }
  | exp EQ exp        { BinOp { op=Eq;  e1=$1; e2=$3 } }
  | exp NEQ exp       { BinOp { op=Neq; e1=$1; e2=$3 } }
  | exp GT exp        { BinOp { op=Gt;  e1=$1; e2=$3 } }
  | exp GTE exp       { BinOp { op=Gte; e1=$1; e2=$3 } }
  | exp LT exp        { BinOp { op=Lt;  e1=$1; e2=$3 } }
  | exp LTE exp       { BinOp { op=Lte; e1=$1; e2=$3 } }
  | exp AND exp       { BinOp { op=And; e1=$1; e2=$3 } }
  | exp OR exp        { BinOp { op=Or;  e1=$1; e2=$3 } }

  // 条件式
  // if e1 then e2 else e3
  // if e1 then e2
  | IF e1=exp THEN e2=exp ELSE e3=exp { IfExp { cond=e1; th=e2; el=Some(e3) } }
  | IF e1=exp THEN e2=exp             { IfExp { cond=e1; th=e2; el=None } }

  // ループ
  // while e1 do e2
  // for id := e1 to e2 do e3
  // break
  | WHILE cond=exp DO body=exp
    { WhileExp { cond=cond; body=body } }
  | FOR id=ID ASSIGN e1=exp TO e2=exp DO e3=exp
    { ForExp { var=id; lo=e1; hi=e2; body=e3 } }
  | BREAK
    { BreakExp }

  // let DECS in EXP end
  | LET decs=list(dec) IN exp=exp END
    { LetExp { decs=decs; body=exp } }

// Declarations
dec :
  | separated_nonempty_list(AND, tydec) { TyDec $1 }
  | separated_nonempty_list(AND, fundec) { FunDec $1 }
  | vardec { $1 }

// type ID = TY
tydec:
 | TYPE x=ID EQ user_ty=ty { { tyname=x; ty=user_ty } }

// var x:TY := EXP
// var x := EXP
vardec:
  | VAR x=ID var_ty=type_constraint ASSIGN e1=exp
    { VarDec { var_name=x; var_type=var_ty; init_val=e1 } }

// :TY
type_constraint:
  | c=option(COLON t=ID { t })
    { c }

// function f (PARAMS) : TY = EXP
fundec:
  | FUNCTION name=ID LPAREN params=separated_list(COMMA, tyfield) RPAREN res_ty=type_constraint EQ body=exp
    { { name=name; params=params; result_type=res_ty; body=body } }

field :
  | x=ID EQ e=exp
    { (x, e) }

tyfield :
  | name=ID COLON ty=ID
    { { field_name=name; field_type=ty } }

ty:
  | ty=ID
    { NameTy ty }
  | LBRACE fields=separated_list(COMMA, tyfield) RBRACE
    { RecordTy fields }
  | ARRAY OF ty=ID
    { ArrayTy ty }