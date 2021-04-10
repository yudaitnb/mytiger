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
let rec check_exp (((venv : E.venv), (tenv : E.tenv), inloop) as env) exp =
  match exp with
  | A.IntExp _    -> T.INT
  | A.BoolExp _   -> T.INT
  | A.StringExp _ -> T.STRING
  | NilExp _      -> T.NIL

  (* 二項演算 *)
  | A.BinOpExp { op; e1; e2; loc; } -> (
    let tye1 = check_exp env e1 in
    let tye2 = check_exp env e2 in
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
   * 左辺値
   * 値環境中でその変数に割り当てられた型がその値の型
   *)
  | VarExp { var; _ } ->
    let tyvar = check_var venv var in
    tyvar
    
  (*
   * 代入式
   * lvalue : = exp
   * lvalueを評価してからexpを評価する。
   * lvalueの内容に式の結果を設定する
   * 値を生成しない
   *)
  | AssignExp { var; exp; loc } -> 
    let tylvalue = check_var venv var in
    let tyexp = check_exp env exp in
    coerce tylvalue tyexp loc;
    T.UNIT

  (* 
   * if-exp
   * - else節が存在するなら、二つの返り値型tythとtyelは同じ型
   *)
  | IfExp { cond; th; el; loc } -> (
    let tycond = check_exp env cond in
    let tyth   = check_exp env th in
    check_int tycond loc; (
    match el with
    | None -> tyth
    | Some el' -> 
      let tyel = check_exp env el' in 
      coerce tyth tyel loc;
      tyth
    )
  )

  (* 
   * While式 
   * - bodyは値を返さない
   *)
  | WhileExp { cond; body; loc } ->
    let tycond = check_exp env cond in
    let tybody = check_exp (venv, tenv, true) body in
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
    let tylo   = check_exp env lo in
    let tyhi   = check_exp env hi in
    let tybody = check_exp (ex_venv, tenv, true) body in
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
   * - 型環境中に存在するレコード型T.RECORD {(S.symbol * ty) list * unique}中の対応するフィールドの要素型ty
   *)
  | RecordExp { record_name; record_fields; loc; } -> (
    let t_rec_fields = 
      List.map
        (fun (_, field) -> check_exp env field)
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
      (* 各フィールドの型が等しいか検査 *)
      (List.iter2
        (fun a b -> coerce a b loc)
        (t_rec_fields)
        (maketylist lst));
       t
    | _ -> misdefined loc "array type" (S.symbol record_name)
    end
  )

  (*
   * 配列
   * - 要素型telemと型環境中に存在する配列型T.Array(te,_)の要素型teが等しい
   *)
  | ArrayExp { array_name; size; init; loc; } ->
    let tsize = check_exp env size in
    check_int tsize loc;
    let telem = check_exp env init in
    begin match T.actual_ty (tylook tenv (S.symbol array_name) loc) with
    | T.ARRAY (te,_) as t ->
       coerce telem te loc;
       t
    | _ -> misdefined loc "array type" (S.symbol array_name)
    end

  (*
   * 式列
   * 最後の式の値が返り値
   *)
  | SeqExp explist ->
    let ty = List.fold_left
      (fun _ exp ->
        let t = check_exp env exp in
        t)
      (T.UNIT)
      (explist)
    in
      ty

  (*
   * Let式
   * let decs in body end
   * - decsを全て環境に追加し、その環境でbodyをの型検査を行う
   * - let式の返り値はbodyの返り値
   *)
  | LetExp { decs; body; _; } ->
    let venv', tenv' = List.fold_left
      (fun (acc_venv, acc_tenv) dec ->
        let new_env = check_dec (acc_venv, acc_tenv) dec in
        new_env)
      (venv, tenv)
      (decs)
    in
      let t = check_exp (venv', tenv', false) body in
      t
  
  | _ -> Error.fatal "unimplemented"

and check_var (venv : E.venv) var =
  match var with
  | A.SimpleVar { name; loc } -> (
    let sym = S.symbol name in
    match varlook venv sym loc with
    | E.FunEntry _      -> misdefined loc "variable" sym
    | E.VarEntry { ty } -> T.actual_ty ty
  )
  | _ -> Error.fatal "not implemented"

(* decを検査し、宣言を環境に追加した新しい環境を返す *)
and check_dec (((venv : E.venv), (tenv : E.tenv)) as env) dec : (E.venv * E.tenv) =
  match dec with
  | A.VarDec { var_name; var_type; init_val; loc } ->
    begin
      let tyinitval = check_exp (venv, tenv, false) init_val in
      let varsym = S.symbol var_name in
      let tyvar = (
        match var_type with
        (* 型制約有り -> initvalの型と一致することを検査*)
        | Some name ->
          let tyvarsym = S.symbol name in
          let t = tylook tenv tyvarsym loc in
          coerce t tyinitval loc;
          t
        (* 型制約無し*)
        | None -> tyinitval)
      in
        let entry = E.VarEntry { ty = tyvar } in
        let venv_new = S.enter varsym entry venv in
        (venv_new, tenv)
    end
  | A.TypeDec tydecs -> 
    begin
      let env_new = List.fold_left
        (fun (_, acctenv) ({ tyname; ty; loc } : A.tydec) ->
          let tyvarsym = S.symbol tyname in
          let checkedty = check_ty (venv,tenv) ty in
          let tenv' = S.enter tyvarsym checkedty acctenv in
          (venv, tenv'))
        (env)
        (tydecs)
      in
        env_new
    end
  (*
   * FunDec  of fundec list
   * fundec = {
   *   name : name;
   *   params : field list;
   *   result_type : name option;
   *   body : exp;
   *   loc : location; [@equal fun _ _ -> true]
   * }
   * FunEntry of { formals : T.ty list; result : T.ty }
   * 引数リストparamsをT.ty listにする
   * これと結果型を使ってFunEntry {formals;result}を生成
   *)
  | A.FunDec fundecs -> 
    let make_entry ({ name; params; result_type; body; loc } : A.fundec) venv =
      let symtylist = List.map
        (fun ({ field_name; field_type } : A.field) ->
          let sym_field_name = S.symbol field_name in
          let sym_field_type = S.symbol field_type in
          let t = tylook tenv sym_field_type loc in
          (sym_field_name, t))
        (params) in
      let tylist = List.map
        (fun (_,t) -> t)
        (symtylist) in
      match result_type with
      | None -> Error.fatal "not implemented"
      | Some resty -> 
        let sym_resty = S.symbol resty in
        let t_resty = tylook tenv sym_resty loc in
        let newentry = E.FunEntry { formals = tylist; result = t_resty } in
        let venv'  = S.enter (S.symbol name) newentry venv in
        let venv'' = venv' (*TODO*) in
        (venv', venv'')
    in
    let venv'_res =List.fold_left
      (fun acc_venv fundec ->
        let (venv', venv'') = make_entry fundec acc_venv in
        venv')
      (venv)
      (fundecs) in
    (venv'_res, tenv)
      
(* 
 * type a = b
 * type any = {any : int}
 * type intArray = array of int
 * | NameTy   of name * location -> b
 * | RecordTy of field list      -> RECORD of (Symbol.symbol * ty) list * unique
 * | ArrayTy  of name * location -> ARRAY of ty * unique
 *)
and check_ty (((venv : E.venv), (tenv : E.tenv)) as env) ty =
  match ty with
  | A.NameTy (name,loc) -> 
    let symty = S.symbol name in
    let ty = tylook tenv symty loc in
    T.actual_ty ty
  | A.RecordTy fieldlist -> 
    let recordty = List.fold_left
      (fun acc ({ field_name; field_type } : A.field) ->
        let sym_field_name = S.symbol field_name in
        let sym_field_type = S.symbol field_type in
        let t = tylook tenv sym_field_type Location.dummy_loc(*TODO*) in
        (sym_field_name, t)::acc)
      ([])
      (fieldlist)
    in
      T.RECORD (recordty, ref ())
  | A.ArrayTy (name,loc) ->
    let symty = S.symbol name in
    let ty = tylook tenv symty loc in
    T.ARRAY (T.actual_ty ty, ref ())

let typeof program =
  check_exp (E.base_venv, E.base_tenv, false) program