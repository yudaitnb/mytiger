open Ast
(* open Hashtbl
open Stack *)

type value = [
  | `VInt  of int
  | `VBool of bool
  | `VString of string
  (* | `VUnit *)
  | `VRecord of value list
  | `VArray  of value list ]
  [@@ deriving show, eq]

let get_value value = 
  match value with
  | `VInt  i -> i
  | `VBool b -> b
  | `VString s -> s
  (* | `VUnit  -> unit *)
  | `VRecord values -> values
  | `VArray  values -> values

(* let table_evaluator = Hashtbl.create 100

let env = Stack.create *)

let rec eval exp stack env = 
  match exp with
  | IntExp v    -> `VInt v.value
  | BoolExp v   -> `VBool v.value
  | StringExp v -> `VString v.value
  (* | BinOp { op : binop; e1 : exp; e2 : exp; _; } -> (
    let v1 = eval e1 stack env in
    let v2 = eval e2 stack env in
    match op with
    | Add ->  VInt (v1  + v2)
    | Sub ->  VInt (v1  - v2)
    | Mul ->  VInt (v1  * v2)
    | Div ->  VInt (v1  / v2)
    | Eq  ->  VInt (v1  = v2)
    | Neq -> VBool (v1 <> v2)
    | Lt  -> VBool (v1  < v2)
    | Lte -> VBool (v1 <= v2)
    | Gt  -> VBool (v1  > v2)
    | Gte -> VBool (v1 >= v2)
    | And -> VBool (v1 && v2)
    | Or  -> VBool (v1 || v2) ) *)
  | UnOpExp { op : unop; e : exp; _; } -> (
    let v = eval e stack env in
    match op with
    | Minus -> (
      match v with
      | `VInt i -> `VInt ( -i )
      | _       -> failwith "not int" )
    | Not   -> (
      match v with
      | `VBool b -> `VBool ( not b )
      | _        -> failwith "not bool" ) 
    )
  | IfExp { cond : exp; th : exp ; el : exp option; _; } -> (
    let res_cond = eval cond stack env in
    if equal_value res_cond (`VBool true)
      then eval th stack env
      else ( match el with
        | Some x -> eval x stack env
        | None   -> failwith "a"
    ) )
  (* ループ *)
  (* | WhileExp  { cond : exp; body : exp; pos : position; } -> (
    let res_cond = (eval cond stack env).value in
    if res_cond
      then VInt 1
      else VInt 1 )
  | ForExp    { var : name; lo : exp; hi : exp; body : exp; pos : position; }
    -> VInt 1
  | BreakExp  { pos : position; }
    -> VInt 1
  (* レコード *)
  | Nil
    -> VInt 1
  | RecordExp { record_fields : (name * exp) list; record_type : name; pos : position; }
    -> VInt 1
  | DotExp    { record : exp; label : name; pos : position; }
    -> VInt 1
  (* 配列 *)
  | ArrayExp { size : exp; init : exp; array_type : name; pos : position; }
    -> VInt 1
  (* 列化 *)
  | SeqExp exps
    -> VInt 1
  | LetExp { decs : (dec node) list; body : exp; pos : position; }
    -> VInt 1 *)
  | EOF -> `VInt 1
  | _   -> `VInt 1