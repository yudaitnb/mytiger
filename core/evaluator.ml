(* open Ast
open Hashtbl

type value =
  | VInt  of int
  | VBool of bool
  | VString of string
  | VUnit
  | VRecord of value list
  | VArray  of value list

let table_evaluator = Hashtbl.create 100

let makenode id loc value = {id = id; loc = loc; value = value}

let rec eval nexp = 
  let id = nexp.id in
  let child_exp = nexp.value in
  let c_loc = nexp.loc in
  let pos_s = (fst nexp.loc).pos_cnum in
  let pos_e = (snd nexp.loc).pos_cnum in
  match child_exp with
  | Lit v -> makenode id c_loc (Lit v)
  (* | BinOp { op : binop; e1 : exp node; e2 : exp node } -> (
    let res_e1 = (eval e1).value in
    let res_e2 = (eval e2).value in
    match op with
    | Add -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
    | Sub -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
    | Mul -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
    | Div -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
    | Eq  -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
    | Neq -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
    | Lt  -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
    | Lte -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
    | Gt  -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
    | Gte -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
    | And -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
    | Or  -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
  | UnOp  { op : unop; e : exp node }
    -> 
  | IfExp of { cond : exp node; th : exp node ; el : exp node option }
    -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
  (* ループ *)
  | WhileExp  of { cond : exp node; body : exp node }
    -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
  | ForExp    of { var : name; lo : exp node; hi : exp node; body : exp node }
    -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
  | BreakExp
    -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
  (* レコード *)
  | Nil
    -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
  | RecordExp of { record_fields : (name * exp node) list; record_type : name }
    -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
  | DotExp    of { record : exp node; label : name }
    -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
  (* 配列 *)
  | ArrayExp of { size : exp node; init : exp node; array_type : name }
    -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
  (* 列化 *)
  | SeqExp of exp node list
    -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
  | LetExp { decs : (dec node) list; body : exp node }
    -> makenode id c_loc (makenode id c_loc (Lit (Int 1))) *)
  | EOF -> makenode id c_loc (makenode id c_loc (Lit (Int 1)))
  | _   -> makenode id c_loc (makenode id c_loc (Lit (Int 1))) *)