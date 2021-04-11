open Location

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

(* type unop =
  | Minus
  | Not
  [@@deriving show, eq] *)


type var =
  | SimpleVar of    { name : name; loc : location; }
  | FieldVar of     { var : var; name : name; loc : location; }
  | SubscriptVar of { var : var; exp : exp; loc : location; }
[@@deriving show, eq]

and exp =
  | IntExp of    { value : int; loc : location; }
  | StringExp of { value : string; loc : location; }
  (* 変数 *)
  | VarExp of    { var : var; loc : location; }
  (* 関数呼び出し *)
  | CallExp of   { func : name; args : exp list; loc : location }
  (* 代入式 *)
  | AssignExp of { var : var; exp : exp; loc : location; }
  (* 二項演算 *)
  | BinOpExp of  { op : binop; e1 : exp; e2 : exp; loc : location; }
  (* if式 *)
  | IfExp of     { cond : exp; th : exp ; el : exp option; loc : location; }
  (* ループ *)
  | WhileExp  of { cond : exp; body : exp; loc : location; }
  | ForExp    of { var : name; lo : exp; hi : exp; body : exp; loc : location; }
  | BreakExp  of { loc : location; }
  (* レコード *)
  | NilExp    of { loc : location; }
  | RecordExp of { record_name : name; record_fields : (name * exp) list; loc : location; }
  (* 配列 *)
  | ArrayExp of  { array_name : name; size : exp; init : exp; loc : location; }
  (* 列化 *)
  | SeqExp of exp list
  (* let式 *)
  | LetExp of    { decs : dec list; body : exp; loc : location; }
  [@@deriving show, eq]

and ty =
  | NameTy   of name * location
  | RecordTy of field list
  | ArrayTy  of name * location
  [@@deriving show, eq]

and dec =
  | TypeDec of tydec list
  | VarDec  of {
    var_name : name;
    var_type : name option;
    init_val : exp;
    loc : location;
  }
  | FunDec  of fundec list
  [@@deriving show, eq]

and tydec = {
  tyname : name;
  ty : ty;
  tyloc : location
}
[@@deriving show, eq]

and fundec = {
  name : name;
  params : field list;
  result_type : name option;
  body : exp;
  loc : location;
}
[@@deriving show, eq]

and field = {
  field_name : name;
  field_type : name
}
[@@deriving show, eq]