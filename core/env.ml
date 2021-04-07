module T = Types
module S = Symbol

type enventry =
  | VarEntry of { ty : T.ty }
  | FunEntry of { formals : T.ty list; result : T.ty }

type venv = enventry S.table

type tenv = T.ty S.table

(* 初期型環境生成に使う予約型識別子 *)
let standard_types =
  [ ("int",    T.INT   )
  ; ("string", T.STRING)
  ]

(* 初期型関数環境生成に使う予約関数識別子 *)
let standard_functions =
  [ "printint",  [T.INT                   ], T.UNIT
  ; "exit",      [T.INT                   ], T.UNIT
  (* TODO: complete with the other standard functions *)
  ]

(* standard_functions中の(識別子,引数型,結果型)の組をsymbolテーブルに登録 *)
let base_venv : venv =
  List.fold_left
    (fun env (name, formals, result) ->
      S.enter
        (S.symbol name)
        (FunEntry { formals = formals; result; })
        env)
    S.empty
    standard_functions

(* standard_types中の(識別子,型)の組をsymbolテーブルに登録 *)
let base_tenv : tenv =
  List.fold_left
    (fun env (name, t) -> S.enter (S.symbol name) t env)
    S.empty
    standard_types