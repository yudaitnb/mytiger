module A = Ast
module S = Symbol
module E = Env
module T = Types


let type_mismatch loc expected found =
  Error.error loc "type mismatch: expected %s, but found %s" (T.string_of_ty expected) (T.string_of_ty found)

let undefined loc kind id =
  Error.error loc "undefined %s %s" kind (S.name id)

let misdefined loc kind id =
  Error.error loc "%s is not a %s" (S.name id) kind

let cannot_be_nil loc id =
  Error.error loc "cannot initialize untyped variable %s with nil" (S.name id)

let break_is_not_in_loop loc =
  Error.error loc "breal exp isn't in loop exp"

let coerceable = T.coerceable

let look env kind id loc =
  match S.look id env with
  | Some x -> x
  | None -> undefined loc kind id

let tylook tenv id loc =
  look tenv "type" id loc

let funlook venv id loc =
  look venv "function" id loc

let varlook venv id loc =
  look venv "variable" id loc

let coerce ty1 ty2 loc =
  if not (coerceable ty1 ty2) then
    type_mismatch loc ty2 ty1

let check_int ty loc = coerce ty T.INT loc

let check_unit ty loc = coerce ty T.UNIT loc

module Translate = struct
  type exp = unit 
end

type expty = { exp: Translate.exp; ty: Types.ty }

(* Ast.Expから(Translate.Exp, Types.ty)への変換 *)
let rec check_exp (venv : E.venv) (tenv : E.tenv) inloop exp =
  match exp with
  | A.IntExp _    -> T.INT
  | A.BoolExp _   -> T.INT
  | A.StringExp _ -> T.STRING
  | NilExp _      -> T.NIL

  (* 二項演算 *)
  | A.BinOpExp { op; e1; e2; loc; } -> (
    let tye1 = check_exp venv tenv inloop e1 in
    let tye2 = check_exp venv tenv inloop e2 in
    match op with
    (* 算術二項演算 *)
    | A.Add -> check_int tye1 loc; check_int tye2 loc; T.INT
    | A.Sub -> check_int tye1 loc; check_int tye2 loc; T.INT
    | A.Mul -> check_int tye1 loc; check_int tye2 loc; T.INT
    | A.Div -> check_int tye1 loc; check_int tye2 loc; T.INT

    (* 算術比較演算 *)
    | A.Eq  -> check_int tye1 loc; check_int tye2 loc; T.INT
    | A.Neq -> check_int tye1 loc; check_int tye2 loc; T.INT
    | A.Lt  -> check_int tye1 loc; check_int tye2 loc; T.INT
    | A.Lte -> check_int tye1 loc; check_int tye2 loc; T.INT
    | A.Gt  -> check_int tye1 loc; check_int tye2 loc; T.INT
    | A.Gte -> check_int tye1 loc; check_int tye2 loc; T.INT

    (* 論理二項演算 *)
    | A.And -> check_int tye1 loc; check_int tye2 loc; T.INT
    | A.Or  -> check_int tye1 loc; check_int tye2 loc; T.INT
  )

  (* 
   * if-exp
   * - else節が存在するなら、二つの返り値型tythとtyelは同じ型
   *)
  | IfExp { cond; th; el; loc } -> (
    let tycond = check_exp venv tenv inloop cond in
    let tyth   = check_exp venv tenv inloop th in
    check_int tycond loc; (
    match el with
    | None -> tyth
    | Some el' -> 
      let tyel = check_exp venv tenv inloop el' in 
      coerce tyth tyel loc; tyth
    )
  )

  (* 
   * While式 
   * - bodyは値を返さない
   *)
  | WhileExp { cond; body; loc } ->
    let tycond = check_exp venv tenv false cond in
    let tybody = check_exp venv tenv true body in
    check_int tycond loc; check_unit tybody loc; T.UNIT

  (*
   * For式
   * - bodyは値を返さない
   * - bodyを評価中のみ値環境がvarで拡張される(ex_venv)
   *)
  | ForExp { var; lo; hi; body; loc } ->
    let ex_venv = 
      S.enter 
        (S.symbol var) 
        (E.VarEntry { ty = T.INT }) 
        venv
    in
    let tylo   = check_exp venv tenv inloop lo in
    let tyhi   = check_exp venv tenv inloop hi in
    let tybody = check_exp ex_venv tenv true body in
    check_int tylo loc; check_int tyhi loc; check_unit tybody loc; T.UNIT

  (* 
   * Break文
   * - breakは値を返さない
   * - breakはforループかwhileループの中にのみ出現できる
   *)
  | BreakExp { loc } ->
    if (not inloop)
      then break_is_not_in_loop loc
      else T.UNIT

  (*
   * レコード式
   * let
   *   type hoge = { a:int, b:int }
   *   var x = hoge{ a=1, b=1 }
   * in ...
   * 以下の2つの型が等しい
   * - 各フィールドの型
   * - 型環境中に存在するレコード型T.RECORD {(S.symbol * ty) list * unique}の対応するフィールドの要素型ty
   *)
  | RecordExp { record_name; record_fields; loc; } -> (
    let t_rec_fields = 
      List.map
        (fun (_, field) -> check_exp venv tenv inloop field)
        (record_fields)
    in
    begin match T.actual_ty (tylook tenv (S.symbol record_name) loc) with
    | T.RECORD (lst, _) as t ->
      (* (symbol * ty) list -> ty list *)
      let rec maketylist tpllst =
        match tpllst with
        | [] -> []
        | (_,ty)::rst -> ty :: (maketylist rst)
      in
      (List.iter2
        (fun a b -> coerce a b loc)
        (t_rec_fields)
        (maketylist lst));
       t
    | _ -> misdefined loc "array type" (S.symbol record_name)
    end
  )

  (* レコード取り出し *)
  | DotExp { record; label; loc; }
    -> Error.fatal "unimplemented"

  (*
   * 配列
   * - 要素型telemと型環境中に存在する配列型T.Array(te,_)の要素型teが等しい
   *)
  | ArrayExp { array_name; size; init; loc; } ->
    let tsize = check_exp venv tenv inloop size in
    check_int tsize loc;
    let telem = check_exp venv tenv inloop init in
    begin match T.actual_ty (tylook tenv (S.symbol array_name) loc) with
    | T.ARRAY (te,_) as t ->
       coerce telem te loc;
       t
    | _ -> misdefined loc "array type" (S.symbol array_name)
    end

  (* 式列 *)
  | SeqExp explist
    -> Error.fatal "unimplemented"

  (* Let式 *)
  | LetExp { decs; body; loc; }
    -> Error.fatal "unimplemented"
  
  | EOF -> T.UNIT
  | _ -> Error.fatal "unimplemented"
