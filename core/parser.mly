%{
open Ast
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
// %token AT

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

// EOF
%token EOF

// 演算子の結合順位（結合順位が低い順）
%nonassoc DO
%nonassoc ASSIGN
%nonassoc OF

// if c1 then if c2 then e1 else e2
// = if c1 then (if c2 then e1 else e2)
%nonassoc THEN
%nonassoc ELSE

%left OR
%left AND
%nonassoc EQ NEQ LT LTE GT GTE
%left PLUS MINUS
%left ASTERISK SLASH

%nonassoc UMINUS

%start <exp> prog

%%

// プログラムは大きな一つの式
prog:
  | exp EOF { $1 }

lvalue:
  | v=ID
    { SimpleVar { name = v; loc = $loc } }
  | lvalue_not_id
    { $1 }

lvalue_not_id:
  | v=lvalue DOT f=ID
    { FieldVar { var = v; name = f; loc = $loc } }
  | v=ID LBRACKET e=exp RBRACKET
    { SubscriptVar {
      var = SimpleVar { name = v; loc = $loc };
      exp = e; loc = $loc } }
  | v=lvalue_not_id LBRACKET e=exp RBRACKET
    { SubscriptVar { var = v; exp = e; loc = $loc } }

// 式
exp:
  // リテラル
  | INT
    { IntExp    { value = $1; loc = $loc } }
  | STR
    { StringExp { value = $1; loc = $loc } }

  // レコード
  // nil
  // type_id {v1=e1, .. ,vn=2n}
  // var.label
  | NIL
    { NilExp { loc = $loc } }
  | record_id=ID LBRACE fields=separated_list(COMMA, field) RBRACE
    { RecordExp { record_name=record_id; record_fields=fields; loc = $loc } }

  // 配列
  // type-id [e1] of e2
  | type_id=ID LBRACKET e1=exp RBRACKET OF e2=exp
    { ArrayExp { array_name=type_id; size=e1; init=e2; loc = $loc } }

  // 二項演算
  | exp PLUS exp      { BinOpExp { op=Add; e1=$1; e2=$3; loc = $loc } }
  | exp MINUS exp     { BinOpExp { op=Sub; e1=$1; e2=$3; loc = $loc } }
  | exp ASTERISK exp  { BinOpExp { op=Mul; e1=$1; e2=$3; loc = $loc } }
  | exp SLASH exp     { BinOpExp { op=Div; e1=$1; e2=$3; loc = $loc } }
  | exp EQ exp        { BinOpExp { op=Eq;  e1=$1; e2=$3; loc = $loc } }
  | exp NEQ exp       { BinOpExp { op=Neq; e1=$1; e2=$3; loc = $loc } }
  | exp GT exp        { BinOpExp { op=Gt;  e1=$1; e2=$3; loc = $loc } }
  | exp GTE exp       { BinOpExp { op=Gte; e1=$1; e2=$3; loc = $loc } }
  | exp LT exp        { BinOpExp { op=Lt;  e1=$1; e2=$3; loc = $loc } }
  | exp LTE exp       { BinOpExp { op=Lte; e1=$1; e2=$3; loc = $loc } }
  | exp AND exp       { BinOpExp { op=And; e1=$1; e2=$3; loc = $loc } }
  | exp OR exp        { BinOpExp { op=Or;  e1=$1; e2=$3; loc = $loc } }

  // 単項マイナス
  | MINUS e=exp %prec UMINUS
    { BinOpExp { op=Sub; e1=IntExp {value=0;loc=$loc;}; e2=e; loc = $loc } }

  // 条件式
  // if e1 then e2 else e3
  // if e1 then e2
  | IF e1=exp THEN e2=exp             
    { IfExp { cond=e1; th=e2; el=None; loc = $loc } }
  | IF e1=exp THEN e2=exp ELSE e3=exp 
    { IfExp { cond=e1; th=e2; el=Some(e3); loc = $loc } }

  // ループ
  // while e1 do e2
  // for id := e1 to e2 do e3
  // break
  | WHILE cond=exp DO body=exp
    { WhileExp { cond=cond; body=body; loc = $loc } }
  | FOR id=ID ASSIGN e1=exp TO e2=exp DO e3=exp
    { ForExp { var=id; lo=e1; hi=e2; body=e3; loc = $loc } }
  | BREAK
    { BreakExp { loc = $loc } }

  // 変数
  | v=ID
    { VarExp {
      var = SimpleVar { name = v; loc = $loc };
      loc = $loc } }

  // 左辺値(変数含)
  | lvalue_not_id
    { VarExp { var = $1; loc = $loc  } }

  // 代入式
  | lvalue=lvalue ASSIGN e=exp
    { AssignExp { var = lvalue; exp = e; loc = $loc } }

  // 関数呼び出し
  | func=ID LPAREN args = separated_list(COMMA, exp) RPAREN
    { CallExp { func = func; args = args; loc = $loc} }

  // let式
  // let DECS in EXP end
  | LET decs=declist IN exp=exp END
    { LetExp { decs=decs; body=exp; loc = $loc } }
  // EXPが()無しシークエンスになっているプログラムへの対応
  // let DECS in e1;...;en end
  | LET decs=declist IN expseq=expseq END
    { LetExp { decs=decs; body=(SeqExp expseq); loc = $loc } }

  // シ－クエンス
  // (e1; e2; ... ; en)
  //  - 一要素のときは括弧
  //  - 二要素以上でシークエンス
  //  - 終端に余計なセミコロンを許容する (e1; e2;)
  | LPAREN exp=exp RPAREN
    { exp }
  | LPAREN expseq=expseq RPAREN
    { SeqExp expseq }

// シークエンス
expseq:
  | e1=exp SEMICOLON e2=exp           { [e1; e2] }
  // | e1=exp SEMICOLON e2=exp SEMICOLON { [e1; e2] }
  | top=exp SEMICOLON rest=expseq     { top :: rest }

// Declarations
//  - fundecとtydecは隣接する定義は一纏めにする
declist:
  | tydecs=nonempty_list(tydec)
    { [TypeDec tydecs] }
  | fundecs=nonempty_list(fundec)
    { [FunDec fundecs] }
  | vardec=vardec
    { [vardec] }
  | top=nonempty_list(tydec) rest=nontydeclist
    { (TypeDec top) :: rest }
  | top=nonempty_list(fundec) rest=nonfundeclist
    { (FunDec top) :: rest }
  | top=vardec rest=declist
    { top :: rest }

nontydeclist:
  | top=nonempty_list(fundec)
    { [FunDec top] }
  | top=vardec
    { [top] }
  | top=nonempty_list(fundec) rest=nonfundeclist
    { (FunDec top) :: rest }
  | top=vardec rest=declist
    { top :: rest }

nonfundeclist:
  | top=nonempty_list(tydec)
    { [TypeDec top] }
  | top=vardec
    { [top] }
  | top=nonempty_list(tydec) rest=nontydeclist
    { (TypeDec top) :: rest }
  | top=vardec rest=declist
    { top :: rest }

// type ID = TY
tydec:
  | TYPE x=ID EQ user_ty=ty
    { { tyname=x; ty=user_ty; tyloc = $loc } }

// var x:TY := EXP
// var x := EXP
vardec:
  | VAR x=ID var_ty=type_constraint ASSIGN e1=exp
    { VarDec { var_name = x; var_type = var_ty; init_val = e1; loc = $loc } }

// :TY
type_constraint:
  | c=option(COLON t=ID { t })
    { c }

// function f (PARAMS) : TY = EXP
fundec:
  | FUNCTION name=ID LPAREN params=separated_list(COMMA, tyfield) RPAREN res_ty=type_constraint EQ body=exp
    { { name=name; params=params; result_type=res_ty; body=body; loc = $loc } }

field :
  | x=ID EQ e=exp
    { (x, e) }

tyfield :
  | name=ID COLON ty=ID
    { { field_name=name; field_type=ty } }

ty:
  | ty=ID
    { NameTy (ty, $loc) }
  | LBRACE fields=separated_list(COMMA, tyfield) RBRACE
    { RecordTy fields }
  | ARRAY OF ty=ID
    { ArrayTy (ty, $loc) }
