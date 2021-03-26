type pos = Int
type name = string [@@deriving show, eq]

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | And
  | Or
  [@@deriving show, eq]

type unop =
  | Minus
  | Not
  [@@deriving show, eq]

type literal =
  | Int of int
  | Bool of bool
  | Str of string
  | Unit
  [@@deriving show, eq]

type exp =
  | Lit of literal
  | Var of name
  | BinOp of { op : binop; e1 : exp; e2 : exp }
  | UnOp of { op : unop; e : exp }
  | IfExp of { cond : exp; th : exp ; el : exp option }
  (* ループ *)
  | WhileExp  of { cond : exp; body : exp }
  | ForExp    of { var : name; lo : exp; hi : exp; body : exp }
  | BreakExp
  (* レコード *)
  | Nil
  | RecordExp of { record_fields : (name * exp) list; record_type : name }
  | DotExp of    { record : exp; label : name }
  (* 配列 *)
  | ArrayExp of { size : exp; init : exp; array_type : name }
  (* | SeqExp    of lexp list *)
  | EOF
  | LetExp of { decs : dec list; body : exp }
  [@@deriving show, eq]

and dec =
  | TyDec     of tydec list
  | VarDec      of vardec
  | FunDec of fundec list
  [@@deriving show]

and ty =
  | NameTy   of name
  | RecordTy of field list
  | ArrayTy  of name
  [@@deriving show, eq]

and tydec = {
  tyname : name;
  ty : ty;
  (* typos : pos *)
}
[@@deriving show, eq]

and vardec = {
  var_name : name;
  var_type : name option;
  init_val : exp(*; pos : pos*)
}
[@@deriving show, eq]

and fundec = {
  name : name;
  params : field list;
  result_type : name option;
  body : exp(*; funpos : pos; *)
}
[@@deriving show, eq]

and field = {
  field_name : name;
  field_type : name
  (* pos : pos *)
}
[@@deriving show, eq]
