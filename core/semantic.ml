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

let field_misdefined loc id =
  Error.error loc "Some record fields are undefined: %s" (S.name id)

let break_is_not_in_loop loc =
  Error.error loc "breal exp isn't in loop exp"

let is_comparable loc ty =
  if not (T.comparable ty) then
  Error.error loc "A value of type %s is not comaparable type" (T.show_ty ty)

let includes_cyclic_definitions loc id =
  Error.error loc "%s includes a cyclic type definition." (S.name id)

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

let check_string ty loc = coerce ty T.STRING loc

let check_unit ty loc = coerce ty T.UNIT loc

let symlist (tydecs : A.tydec list) =
  List.map 
    (fun (tydec : A.tydec) -> S.symbol tydec.tyname)
    tydecs

module Translate = struct
  type exp = unit 
end

type expty = { exp: Translate.exp; ty: Types.ty }

(* Ast.Expから(Translate.Exp, Types.ty)への変換 *)
let rec check_exp (((venv : E.venv), (tenv : E.tenv), inloop) as env) exp =
  match exp with
  | A.IntExp _    -> T.INT
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

    (* 比較演算 *)
    | A.Eq  -> is_comparable loc tye1; is_comparable loc tye2; T.INT
    | A.Neq -> is_comparable loc tye1; is_comparable loc tye2; T.INT
    | A.Lt  -> is_comparable loc tye1; is_comparable loc tye2; T.INT
    | A.Lte -> is_comparable loc tye1; is_comparable loc tye2; T.INT
    | A.Gt  -> is_comparable loc tye1; is_comparable loc tye2; T.INT
    | A.Gte -> is_comparable loc tye1; is_comparable loc tye2; T.INT

    (* 論理二項演算 *)
    | A.And -> is_comparable loc tye1; is_comparable loc tye2; T.INT
    | A.Or  -> is_comparable loc tye1; is_comparable loc tye2; T.INT
  )

  (*
   * 左辺値
   * - 値環境中でその変数に割り当てられた型がその値の型
   *)
  | VarExp { var; _ } ->
    let tyvar = check_var env var in
    tyvar

  (* 
   * 関数呼び出し 
   * CallExp { func : name; args : exp list; loc : location }
   * - 関数名nameに対応する環境中のFunEntry { formals; result }
   * - 引数の型リストargsとFunEntryの引数リストformalsの型が等しいか検査
   * - 関数呼び出しの型は関数の結果型result
   *)
  | CallExp { func; args; loc } ->
    let tyfunc = funlook venv (S.symbol func) loc in
    begin match tyfunc with
    | VarEntry _ -> misdefined loc "function" (S.symbol func)
    | FunEntry { formals; result } -> 
      (List.iter2
        (fun a b -> coerce a b loc)
        (List.map (check_exp env) args)
        (formals));
      T.actual_ty result
    end
    
  (*
   * 代入式
   * lvalue : = exp
   * lvalueを評価してからexpを評価する
   * lvalueの内容に式の結果を設定する
   * 値を生成しない
   *)
  | AssignExp { var; exp; loc } -> 
    let tylvalue = check_var env var in
    let tyexp = check_exp env exp in
    coerce tylvalue tyexp loc;
    T.UNIT

  (* 
   * if-exp
   * - else節が存在しないなら、if-expは値を返さない
   * - else節が存在するなら、then/else節の型tythとtyelは等しく、if-expの型もそれに等しい
   *)
  | IfExp { cond; th; el; loc } -> (
    let tycond = check_exp env cond in
    let tyth = check_exp env th in
    check_int tycond loc;
    match el with
    | None ->
      (* then節に対してerrorを告知したいが、現在はif-then節全体について告知している。 *)
      coerce tyth T.UNIT loc;
      T.UNIT
    | Some el' -> 
      let tyel = check_exp env el' in 
      coerce tyth tyel loc;
      tyth

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
   * - bodyを評価中はinloopがtrue
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
   * Break
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
   *   var x = hoge{ a=1, b=1 } <<--
   * in ...
   * 以下の2つの型が等しい
   * - 各フィールドの型
   * - 型環境中に存在するレコード型中の対応するフィールドの要素型ty
   *)
  | RecordExp { record_name; record_fields; loc; } -> begin
    (* 各フィールドの型をexpから計算 *)
    let t_rec_fields = 
      List.map
        (fun (sym, field) -> (sym, check_exp env field))
        (record_fields)
    in
    (* 各フィールドの型が等しいか検査 *)
    let defty = T.actual_ty (tylook tenv (S.symbol record_name) loc) in
    match defty with
    | T.RECORD (lst, _) as t ->
      (List.iter2
        (fun (_,a) (_,b) -> coerce a b loc)
        (t_rec_fields)
        (lst));
      t
    | _ -> misdefined loc "array type" (S.symbol record_name)
  end

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
      (fun acc_env dec ->
        let new_env = check_dec acc_env dec in
        new_env)
      (venv, tenv)
      (decs)
    in
      let t = check_exp (venv', tenv', false) body in
      t

and check_var env var =
  let (venv, _, _) = env in
  match var with

  (* 値変数の型 *)
  | A.SimpleVar { name; loc } -> begin
    let sym = S.symbol name in
    match varlook venv sym loc with
    | E.VarEntry { ty } -> T.actual_ty ty
    | E.FunEntry _      -> misdefined loc "variable" sym
  end

  (* レコードフィールドの型 *)
  | A.FieldVar { var; name; loc } -> begin
    let tyvar = T.actual_ty (check_var env var) in
    let sym_recname = S.symbol name in
    match tyvar with
    | T.RECORD (lst, _) -> begin
      match S.looklist sym_recname lst with
      | None -> field_misdefined loc sym_recname
      | Some ty -> ty
    end
    | _ -> misdefined loc "record" sym_recname
  end

  (* 配列の添え字呼び出しの型 *)
  | A.SubscriptVar { var; exp; loc } -> begin
    let tyvar = check_var env var in
    let tyexp = check_exp env exp in
    match tyvar with
    | T.ARRAY (ty, _) -> coerce ty tyexp loc; ty
    | _ -> Error.error loc "on Subscriptvar"
  end

(* decを検査し、宣言を環境に追加した新しい環境を返す *)
and check_dec ((venv : E.venv), (tenv : E.tenv)) dec : (E.venv * E.tenv) =
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
      let symlst = symlist tydecs in
      (* 
       * tydecsを使ってtenvのheaderを生成
       * - 再帰型の型定義に必要
       * - 事前に型の名前だけ含むエントリを入れておく
       * - NAME (symbol a,~) は "型変数aの型" を意味する
       * type a = b (tyexp) -> NAME (symbol a, {contents=None})
       *)
      let tenv_inc_header = List.fold_left
        (fun acc_tenv (tydec : A.tydec) ->
          let tyname = tydec.tyname in
          let tyvarsym = S.symbol tyname in
          let tyheader = T.NAME (tyvarsym, ref None) in
          let tenv' = S.enter tyvarsym tyheader acc_tenv in
          tenv')
        (tenv)
        (tydecs) in
      (* 
       * ヘッダーで拡張された型環境tenv_headerで宣言中の型式を検査 
       * 型式tyexpの示す型が明らかになった時、型環境tenv中のNAME型は型式が示す型に置き換えられる
       * check_ty b = NAME (symbol b, {contents=None}) = newty
       * S.enter (symbol a) newty tenv_header
       *)
      let tenv_inc_dummy = List.fold_left
        (fun (acctenv) ({ tyname; ty; _(*loc*) } : A.tydec) ->
          let tyvarsym = S.symbol tyname in
          let checkedty = check_ty (venv, tenv_inc_header) ty in
          let tenv' = S.enter tyvarsym checkedty acctenv in
          (tenv'))
        (tenv_inc_header)
        (tydecs)
      in
      (* 
       * 最後に型環境dummy_env中のダミーNAME型(= 実体がNone)を解決し、実体型にする (RECORDとARRAYの要素型は除く)
       * 課題1. 型環境のトップレベルにダミーNAME型を含んだまま
       *   - e.g. a -> NAME("b", None), b -> INT を a -> INT, b -> INT に解決したい
       *   - 循環型定義を含んでいたら解決できない
       * 課題2. レコードの要素型にダミーNAME型を含んだまま
       *   - list -> e.g. RECORD([ ("first", INT), ("rest", NAME("list", None) ) ], unique_id)
       *)

      (* 
       * scan : tenv中でcurから辿れる終端の型と、そこまでのsymbol列のペアを返す
       * ひとまずレコードは考えない
       * a -> b
       * b -> c
       * c -> int
       * => (int, [a,b,c])
       * symがscannedに入っていたら循環定義
       *)
      let rec scan (cur : S.symbol) (tenv : E.tenv) (scanned : S.symbol list) loc =
        let ty = tylook tenv cur loc in
        match ty with
        | NAME (sym,{contents=None}) -> 
          if List.exists (fun s -> S.equal_symbol sym s) scanned
            then includes_cyclic_definitions loc cur
            else scan sym tenv (sym::scanned) loc
        | ty -> (ty, scanned)
      in
      (* 型tyとsymbol列を受け取り、tenv中の(symb \in symbol列)をキーにもつ全ての型をtyに更新する *)
      let update_tenv (ty : T.ty) (scanned : S.symbol list) (tenv : E.tenv) =
        List.fold_left
          (fun acctenv sym -> S.enter sym ty acctenv)
          (tenv)
          (scanned)
      in
      (* symとtenvを受け取り、tenv中でsymから辿れるシンボル列の束縛をその終点の型fixedで更新する *)
      let tenv_resolved_dummy_var =
        List.fold_left
          (fun acctenv s -> 
            let fixedty, scanned = scan s acctenv [s] Location.dummy_loc(*TODO*) in
            update_tenv fixedty scanned acctenv)
          (tenv_inc_dummy)
          (symlst)
      in

      (* 
       * symとfixedtyとty (T.RECORD)を受け取り、ty中の全てのNAME(sym,None)の出現をfixedtyで置換する
       * ひとまず自己再帰だけ処理すればよい
       * type list = { a:int, b:list } -> RECORD ([(a,INT),(b,NAME (list,None))], un)
       *)
      let resolve_dummy_in_record (sym : S.symbol) (fixedty : T.ty) (ty : T.ty) =
        match ty with
        | T.RECORD (symtylst, un) -> 
          let newsymtylst = List.map 
            (fun (s, t) ->
              match t with
              (* レコードの要素型はNAME型を残し、実体をRECORDにする *)
              | T.NAME (symname,{contents=None}) -> 
                if (S.equal_symbol sym symname) 
                  then (s, T.NAME(symname, {contents=Some(fixedty)}))
                  else (s, t)
              | t -> (s, t))
            (symtylst)
          in
            T.RECORD (newsymtylst, un)
        | ty -> ty
      in
      let tenv_resolved = 
        List.fold_left
          (fun acctenv s -> 
            let ty = tylook acctenv s Location.dummy_loc in
            let fixedty = resolve_dummy_in_record s ty ty in
            S.enter s fixedty acctenv)
          (tenv_resolved_dummy_var)
          (symlst)
      in
        (venv, tenv_resolved)
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
  | A.FunDec fundecs -> begin
    (* 引数リストparams : {fn;ft} listを(symbol,ty)のリストに変換する関数 *)
    let makesymtylist params loc =
      List.map
        (fun ({ field_name; field_type } : A.field) ->
          let sym_field_name = S.symbol field_name in
          let sym_field_type = S.symbol field_type in
          let t = T.actual_ty (tylook tenv sym_field_type loc) in
          (sym_field_name, t))
        (params)
    in

    (* fundecに対応するheaderを追加した値環境を生成する関数 *)
    (* 再帰関数の型検査に必要 *)
    let make_venv' (fundec : A.fundec) (venv, tenv) = 
      (* 引数型のリストを生成 *)
      let params = fundec.params in
      let loc = fundec.loc in
      let tylist = List.map snd (makesymtylist params loc) in
      (* ヘッダーFunEntryの生成 *)
      let funentry_header =
        match fundec.result_type with
        | None ->    (* 手続き *)
          E.FunEntry { formals = tylist; result = T.UNIT }
        | Some rt -> (* 関数 *)
          let sym_rt = S.symbol rt in
          let ty_rt = T.actual_ty (tylook tenv sym_rt loc) in
          E.FunEntry { formals = tylist; result = ty_rt }
      in
      (* 関数ヘッダーを追加した値環境venv'を返す *)
      let name = fundec.name in
      let venv' = S.enter (S.symbol name) funentry_header venv in
      venv'
    in

    let check_body_type (fundec : A.fundec) (venv, tenv) =
      (* 引数リストparamsを(symbol,ty)のリストに変換 *)
      let params = fundec.params in
      let loc = fundec.loc in
      let symtylist = makesymtylist params loc in
      (* 引数で拡張された環境venv'' *)
      let venv'' = List.fold_left
        (fun acc_venv (sym, ty) -> 
          S.enter sym (E.VarEntry {ty = ty}) acc_venv)
        (venv)
        (symtylist) in
      (* 引数環境で拡張された値環境venv''でbodyを型検査 *)
      let body = fundec.body in
      let result_type = fundec.result_type in
      let t_body = check_exp (venv'', tenv, false) body in
      match result_type with
      | None ->
        coerce T.UNIT t_body loc
      | Some rt -> 
        let sym_rt = S.symbol rt in
        let ty_rt = T.actual_ty (tylook tenv sym_rt loc) in
        coerce ty_rt t_body loc
    in

    (* fundecsを用いて値環境venvにヘッダーを追加した値環境venv'を生成 *)
    let venv' = 
      List.fold_left
        (fun acc_venv (fundec : A.fundec) ->
          make_venv' fundec (acc_venv, tenv))
        (venv)
        (fundecs) in

    (* 関数本体が型制約を満たすか検査する *)
    List.iter
      (fun fundec -> check_body_type fundec (venv',tenv))
      (fundecs);
    (venv', tenv)
  end  
(* 
 * type a = b
 * type any = {any : int}
 * type intArray = array of int
 * | NameTy   of name * location -> b
 * | RecordTy of field list      -> RECORD of (Symbol.symbol * ty) list * unique
 * | ArrayTy  of name * location -> ARRAY of ty * unique
 *)
and check_ty env ty =
  let (_ : E.venv), (tenv : E.tenv) = env in
  match ty with
  | A.NameTy (name,loc) -> 
    let symty = S.symbol name in
    let ty = tylook tenv symty loc in
    T.actual_ty ty
  | A.RecordTy fieldlist -> 
    let recordty = List.map
      (fun ({ field_name; field_type } : A.field) ->
        let sym_field_name = S.symbol field_name in
        let sym_field_type = S.symbol field_type in
        let t = tylook tenv sym_field_type Location.dummy_loc(*TODO*) in
        (sym_field_name, t))
      (fieldlist)
    in
      T.RECORD (recordty, ref ())
  | A.ArrayTy (name,loc) ->
    let symty = S.symbol name in
    let ty = tylook tenv symty loc in
    T.ARRAY (T.actual_ty ty, ref ())

let typeof program =
  check_exp (E.base_venv, E.base_tenv, false) program
