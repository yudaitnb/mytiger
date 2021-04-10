(* 
 * chap5/types.smlの以下を参考にする。
 * structure Types =
 * struct
 *   type unique = unit ref
 *   datatype ty = 
 *       RECORD of (Symbol.symbol * ty) list * unique
 *     | NIL
 *     | INT
 *     | STRING
 *     | ARRAY of ty * unique
 *	   | NAME of Symbol.symbol * ty option ref
 *	   | UNIT
 * end
 *)

(*
 * レコード型と配列型に付与されるユニーク値
 * フィールドが完全に一致していても新しいレコード型を生成する
 *)
type unique = unit ref
[@@deriving show, eq]

(* 型識別子 *)
type ty =
  (* 値がないことを明示する型 *)
  | UNIT
  (* int型識別子 *)
  | INT
  (* string型識別子 *)
  | STRING
  (* nil型識別子, 全てのレコード型に属する *)
  | NIL
  (* 
   * レコード型識別子
   * - フィールド名の組が同じレコードも区別する
   * - tiger言語は構造的部分型を持たない
   *)
  | RECORD of (Symbol.symbol * ty) list * unique
  (* 
   * 配列型識別子
   * - 要素の型が同じでも別の型
   *)
  | ARRAY of ty * unique
  (*
   * 型定義のプレースホルダ
   * - 名前が既知だが、定義が未定義の型
   * - 相互再帰型に使う
   *)
  | NAME of Symbol.symbol * ty option ref
[@@deriving show, eq]

(* NAME型の参照する型を検索する *)
let rec actual_ty = function
  | NAME (_,{contents=Some t}) -> actual_ty t
  | t                          -> t

(* a, bが等しい型か検査する *)
  let rec coerceable a b =
  match a,b with
  | NAME (_,{contents=Some t}), b                          -> coerceable t b
  | a                         , NAME (_,{contents=Some t}) -> coerceable a t
  | UNIT                      , UNIT                       -> true
  | INT                       , INT                        -> true
  | STRING                    , STRING                     -> true
  | NIL                       , NIL                        -> true
  | NIL                       , RECORD _                   -> true
  | RECORD (_,uniqa)          , RECORD (_,uniqb)           -> uniqa = uniqb
  | ARRAY (_,uniqa)           , ARRAY (_,uniqb)            -> uniqa = uniqb
  | _                                                      -> false


let name = Symbol.name
let map = List.map
(* let mkt = Tree.mkt *)

let rec string_of_ty = function
  | UNIT                      -> "UNIT"
  | INT                       -> "INT"
  | STRING                    -> "STRING"
  | NIL                       -> "NIL"
  | RECORD _                  -> "RECORD"
  | ARRAY _                   -> "ARRAY"
  | NAME (n,_)                -> "NAME " ^ name n

(* let rec tree_of_ty = function
  | UNIT                      -> mkt "UNIT" []
  | INT                       -> mkt "INT" []
  | STRING                    -> mkt "STRING" []
  | NIL                       -> mkt "NIL" []
  | RECORD (fs,_)             -> mkt "RECORD" (map (fun (x,t) -> mkt "Field" [mkt (name x) []; tree_of_ty t]) fs)
  | ARRAY (t,_)               -> mkt "ARRAY" [tree_of_ty t]
  | FUNCTION (formals,result) -> mkt "FUNCTION" [mkt "Args" (map tree_of_ty formals); mkt "Result" [tree_of_ty result]]
  | NAME (n,{contents=mt})    -> mkt ("NAME " ^ name n) (match mt with
                                                         | None -> []
                                                         | Some t -> [tree_of_ty t]) *)