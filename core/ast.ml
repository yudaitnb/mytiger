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
  | IntExp of    { value : int; loc : location [@equal fun _ _ -> true]; }
  | BoolExp of   { value : bool; loc : location [@equal fun _ _ -> true]; }
  | StringExp of { value : string; loc : location [@equal fun _ _ -> true]; }
  (* 変数 *)
  | VarExp of    { name : name; loc : location [@equal fun _ _ -> true]; }
  (* 代入式 *)
  | AssignExp of { var : var; exp : exp; loc : location [@equal fun _ _ -> true]; }
  (* 二項演算 *)
  | BinOpExp of  { op : binop; e1 : exp; e2 : exp; loc : location [@equal fun _ _ -> true]; }
  (* if式 *)
  | IfExp of     { cond : exp; th : exp ; el : exp option; loc : location [@equal fun _ _ -> true]; }
  (* ループ *)
  | WhileExp  of { cond : exp; body : exp; loc : location [@equal fun _ _ -> true]; }
  | ForExp    of { var : name; lo : exp; hi : exp; body : exp; loc : location [@equal fun _ _ -> true]; }
  | BreakExp  of { loc : location [@equal fun _ _ -> true]; }
  (* レコード *)
  | NilExp    of { loc : location [@equal fun _ _ -> true]; }
  | RecordExp of { record_name : name; record_fields : (name * exp) list; loc : location [@equal fun _ _ -> true]; }
  | DotExp    of { record : exp; label : name; loc : location [@equal fun _ _ -> true]; }
  (* 配列 *)
  | ArrayExp of  { array_name : name; size : exp; init : exp; loc : location [@equal fun _ _ -> true]; }
  (* 列化 *)
  | SeqExp of exp list
  (* let式 *)
  | LetExp of    { decs : dec list; body : exp; loc : location [@equal fun _ _ -> true]; }
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
  loc : location; [@equal fun _ _ -> true]
}
[@@deriving show, eq]

and vardec = {
  var_name : name;
  var_type : name option;
  init_val : exp;
  loc : location; [@equal fun _ _ -> true]
}
[@@deriving show, eq]

and fundec = {
  name : name;
  params : field list;
  result_type : name option;
  body : exp;
  loc : location; [@equal fun _ _ -> true]
}
[@@deriving show, eq]

and field = {
  field_name : name;
  field_type : name
}
[@@deriving show, eq]