type position = Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
  [@@deriving show, eq]

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


type exp =
  | IntExp of    { value : int; pos : position [@equal fun _ _ -> true]; }
  | BoolExp of   { value : bool; pos : position [@equal fun _ _ -> true]; }
  | StringExp of { value : string; pos : position [@equal fun _ _ -> true]; }
  | VarExp of    { name : name; pos : position [@equal fun _ _ -> true]; }
  | BinOpExp of  { op : binop; e1 : exp; e2 : exp; pos : position [@equal fun _ _ -> true]; }
  | UnOpExp  of  { op : unop; e : exp; pos : position [@equal fun _ _ -> true]; }
  | IfExp of     { cond : exp; th : exp ; el : exp option; pos : position [@equal fun _ _ -> true]; }
  (* ループ *)
  | WhileExp  of { cond : exp; body : exp; pos : position [@equal fun _ _ -> true]; }
  | ForExp    of { var : name; lo : exp; hi : exp; body : exp; pos : position [@equal fun _ _ -> true]; }
  | BreakExp  of { pos : position [@equal fun _ _ -> true]; }
  (* レコード *)
  | NilExp    of { pos : position [@equal fun _ _ -> true]; }
  | RecordExp of { record_fields : (name * exp) list; record_type : name; pos : position [@equal fun _ _ -> true]; }
  | DotExp    of { record : exp; label : name; pos : position [@equal fun _ _ -> true]; }
  (* 配列 *)
  | ArrayExp of  { size : exp; init : exp; array_type : name; pos : position [@equal fun _ _ -> true]; }
  (* 列化 *)
  | SeqExp of exp list
  | LetExp of    { decs : dec list; body : exp; pos : position [@equal fun _ _ -> true]; }
  | EOF
  [@@deriving show, eq]

and dec =
  | TypeDec of tydec list
  | VarDec  of vardec
  | FunDec  of fundec list
  [@@deriving show, eq]

and ty =
  | NameTy   of name
  | RecordTy of field list
  | ArrayTy  of name
  [@@deriving show, eq]

and tydec = {
  tyname : name;
  ty : ty;
  pos : position; [@equal fun _ _ -> true]
}
[@@deriving show, eq]

and vardec = {
  var_name : name;
  var_type : name option;
  init_val : exp;
  pos : position; [@equal fun _ _ -> true]
}
[@@deriving show, eq]

and fundec = {
  name : name;
  params : field list;
  result_type : name option;
  body : exp;
  pos : position; [@equal fun _ _ -> true]
}
[@@deriving show, eq]

and field = {
  field_name : name;
  field_type : name
}
[@@deriving show, eq]