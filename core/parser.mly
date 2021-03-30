%{
open Ast

let node_id = ref 1
let new_node (v) (s : Lexing.position) (e : Lexing.position) =
  let node = {
    id = !node_id;
    loc = (s, e);
    value = v
  }
  in
    node_id := !node_id + 1;
    node
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
%token <bool> BOOL  // 真偽値

// EOF
%token EOF

// 演算子の結合順位（結合順位が低い順）
%nonassoc DO THEN OF
%nonassoc ELSE

%left OR AND
%nonassoc EQ NEQ LT GT LTE GTE
%left PLUS MINUS
%left ASTERISK SLASH

%left DOT
%nonassoc UMINUS


%start <exp Ast.node> prog

%%

// プログラムは大きな一つの式
prog:
  | exp EOF { $1 }

// 式
exp:
  // リテラル
  | INT
    { new_node (Lit (Int $1)) $startpos $endpos }
  | STR
    { new_node (Lit (String $1)) $startpos $endpos }
  | BOOL
    { new_node (Lit (Bool $1)) $startpos $endpos }

  // レコード
  // nil
  // type_id {v1=e1, .. ,vn=2n}
  // var.label
  | NIL
    { new_node Nil $startpos $endpos }
  | type_id=ID LBRACE fields=separated_list(COMMA, field) RBRACE
    { new_node (RecordExp { record_fields=fields; record_type=type_id }) $startpos $endpos }
  | arr=exp DOT label=ID
    { new_node (DotExp { record=arr; label=label }) $startpos $endpos }

  // 配列
  // type-id [e1] of e2
  | type_id=ID LBRACKET e1=exp RBRACKET OF e2=exp
    { new_node (ArrayExp { size=e1; init=e2; array_type=type_id }) $startpos $endpos }

  // 変数
  | ID                { new_node (Var $1) $startpos $endpos }

  // 二項演算
  | exp PLUS exp      { new_node (BinOp { op=Add; e1=$1; e2=$3 }) $startpos $endpos }
  | exp MINUS exp     { new_node (BinOp { op=Sub; e1=$1; e2=$3 }) $startpos $endpos }
  | exp ASTERISK exp  { new_node (BinOp { op=Mul; e1=$1; e2=$3 }) $startpos $endpos }
  | exp SLASH exp     { new_node (BinOp { op=Div; e1=$1; e2=$3 }) $startpos $endpos }
  | exp EQ exp        { new_node (BinOp { op=Eq;  e1=$1; e2=$3 }) $startpos $endpos }
  | exp NEQ exp       { new_node (BinOp { op=Neq; e1=$1; e2=$3 }) $startpos $endpos }
  | exp GT exp        { new_node (BinOp { op=Gt;  e1=$1; e2=$3 }) $startpos $endpos }
  | exp GTE exp       { new_node (BinOp { op=Gte; e1=$1; e2=$3 }) $startpos $endpos }
  | exp LT exp        { new_node (BinOp { op=Lt;  e1=$1; e2=$3 }) $startpos $endpos }
  | exp LTE exp       { new_node (BinOp { op=Lte; e1=$1; e2=$3 }) $startpos $endpos }
  | exp AND exp       { new_node (BinOp { op=And; e1=$1; e2=$3 }) $startpos $endpos }
  | exp OR exp        { new_node (BinOp { op=Or;  e1=$1; e2=$3 }) $startpos $endpos }

  // 単項マイナス
  | MINUS e = exp %prec UMINUS
    { new_node (UnOp { op=Minus; e=e }) $startpos $endpos }

  // 条件式
  // if e1 then e2 else e3
  // if e1 then e2
  | IF e1=exp THEN e2=exp             
    { new_node (IfExp { cond=e1; th=e2; el=None }) $startpos $endpos }
  | IF e1=exp THEN e2=exp ELSE e3=exp 
    { new_node (IfExp { cond=e1; th=e2; el=Some(e3) }) $startpos $endpos }

  // ループ
  // while e1 do e2
  // for id := e1 to e2 do e3
  // break
  | WHILE cond=exp DO body=exp
    { new_node (WhileExp { cond=cond; body=body }) $startpos $endpos }
  | FOR id=ID ASSIGN e1=exp TO e2=exp DO e3=exp
    { new_node (ForExp { var=id; lo=e1; hi=e2; body=e3 }) $startpos $endpos }
  | BREAK
    { new_node (BreakExp) $startpos $endpos }

  // let式
  // let DECS in EXP end
  | LET decs=list(dec) IN exp=exp END
    { new_node (LetExp { decs=decs; body=exp }) $startpos $endpos }
  
  // 列化
  // (e1; e2; ... ; en)
  | LPAREN es=separated_list(SEMICOLON, exp) RPAREN
    { new_node (SeqExp es) $startpos $endpos }

// Declarations
dec :
  | separated_nonempty_list(AND, tydec)
    { new_node (TyDec $1) $startpos $endpos }
  | separated_nonempty_list(AND, fundec)
    { new_node (FunDec $1) $startpos $endpos }
  | vardec
    { new_node ($1) $startpos $endpos }

// type ID = TY
tydec:
  | TYPE x=ID EQ user_ty=ty
    { { tyname=x; ty=user_ty } }

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