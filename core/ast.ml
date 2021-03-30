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

type literal =
  | Int of int
  | Bool of bool
  | String of string
  | Unit
  [@@deriving show, eq]

type exp =
  | Lit of literal
  | Var of name
  | BinOp of { op : binop; e1 : exp node; e2 : exp node }
  | UnOp  of { op : unop; e : exp node }
  | IfExp of { cond : exp node; th : exp node ; el : exp node option }
  (* ループ *)
  | WhileExp  of { cond : exp node; body : exp node }
  | ForExp    of { var : name; lo : exp node; hi : exp node; body : exp node }
  | BreakExp
  (* レコード *)
  | Nil
  | RecordExp of { record_fields : (name * exp node) list; record_type : name }
  | DotExp    of { record : exp node; label : name }
  (* 配列 *)
  | ArrayExp of { size : exp node; init : exp node; array_type : name }
  (* 列化 *)
  | SeqExp of exp node list
  | LetExp of { decs : (dec node) list; body : exp node }
  | EOF
  [@@deriving show, eq]

and dec =
  | TyDec  of tydec list
  | VarDec of vardec
  | FunDec of fundec list
  [@@deriving show, eq]

and ty =
  | NameTy   of name
  | RecordTy of field list
  | ArrayTy  of name
  [@@deriving show, eq]

and tydec = {
  tyname : name;
  ty : ty
}
[@@deriving show, eq]

and vardec = {
  var_name : name;
  var_type : name option;
  init_val : exp node
}
[@@deriving show, eq]

and fundec = {
  name : name;
  params : field list;
  result_type : name option;
  body : exp node
}
[@@deriving show, eq]

and field = {
  field_name : name;
  field_type : name
}
[@@deriving show, eq]

(*
 * 式や宣言のラッパーレコード
 * id   : node ID
 * lnum : 行番号
 * bol  : 行頭からのオフセット
 * ast  : 式または宣言
 *)

and 'a node = {
  id : int [@opaque] [@equal fun _ _ -> true];
  loc : position * position [@opaque] [@equal fun _ _ -> true];
  value : 'a
}
[@@deriving show, eq]

(* let rec my_show_node node = my_show_exp node.value

and my_show_exp exp =
  match exp with
  | BinOp r -> "{ op = " ^ show_binop r.op ^ "; e1 = " ^ my_show_node r.e1 ^ "; e2 = " ^ my_show_node r.e2 ^ " }"
  | UnOp r -> "{ op = " ^ show_unop r.op ^ "; e = " ^ my_show_node r.e ^ " }"
  | IfExp r -> (match r.el with
    | None -> "{ cond = " ^ my_show_node r.cond ^ "; th = " ^ my_show_node r.th ^ "; el = None }"
    | Some s -> "{ cond = " ^ my_show_node r.cond ^ "; th = " ^ my_show_node r.th ^ "; el = Some(" ^ my_show_node s ^ ") }")
  | WhileExp r -> "{ cond = " ^ my_show_node r.cond ^ "; body = " ^ my_show_node r.body ^ " }"
  | ForExp r -> "{ var = " ^ show_name r.var ^ "; lo = " ^ my_show_node r.lo ^ "; hi = " ^ my_show_node r.hi ^ "; body =" ^ my_show_node r.body ^ " }"
  (* | RecordExp r -> "{ record_fields = " ^ my_show_node r.record_fields ^ "; record_type = " ^ show_name r.record_type ^ " }" *)
    (* of { record_fields : (name * exp node) list; record_type : name } *)
  (* | DotExp    of { record : exp node; label : name }
  | ArrayExp of { size : exp node; init : exp node; array_type : name }
  | SeqExp of exp node list
  | LetExp of { decs : (dec node) list; body : exp node } *)
  | x -> show_exp x *)