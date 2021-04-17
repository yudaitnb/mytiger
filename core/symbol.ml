(* 
 * 記号の処理を行うためのモジュール
 * enter - 新しい束縛を追加（古い表を更新せず、新しい表を生成する
 * look k t - 表tの中で文字列kの束縛bを見つけ、Some(b)を返す(kの束縛が存在しないならNone)
 *)

(* (文字列, 文字列に対応するユニーク値) *)
type symbol = (string * int [@equal fun (a, _) (b, _) -> a = b])
[@@deriving show, eq]

(*
 * 出現済みの異なる文字列のカウント
 * 新しい文字列にはその文字列用の整数としてカウントを付与する
 *)
let nextsym = ref 0

(* 
 * 生成するテーブルの大きさ
 * 初期値、必要に応じて自動で伸長
 *)
let size_hint = 128

(*
 * 記号管理に使うテーブル
 * 大きさはsize_hint
 * 型注釈が必要なことに注意
 *)
let hashtable : (string,int) Hashtbl.t = Hashtbl.create size_hint

(* symbolを受け取り、対応する文字列を返す関数 *)
let name (s, _) = s

(* 
 * hashtableを参照し、文字列nameに対応するsymbolを生成する
 * 1. テーブルにnameが存在する場合 [(name,i) \in hashtable]
 *  -> 対応するユニーク値(カウント)iとの組(name,i)を返す
 * 2. テーブルにnameが存在しない場合 [(name,i) \notin hashtable]
 *  -> ユニーク値(カウント)iを一つ進める
 *  -> テーブルに(name,i)を追加する
 *  -> 組(name,i)を返す
 *)
let symbol name =
  try
    let i = Hashtbl.find hashtable name in
    (name,i)
  with Not_found ->
    let i = !nextsym in
    nextsym := i + 1;
    Hashtbl.add hashtable name i;
    (name,i)

let pp_symbol ppf s =
  Format.fprintf ppf "\"%s\"" (name s)

(*
 * Map.Make
 * - Map.OrderedTypeを受け取り、テーブルの実装を返すfunctor
 * - Map.OrderedTypeはマップキーの型tと、順序付け関数compareの実装が必要
 * https://ocaml.jp/archive/ocaml-manual-3.06-ja/libref/Map.Make.html
 *)
 module Table = Map.Make(
  (* purely applicative (no side-effects) association tables over ordered types *)
  struct
    type t = symbol
    let compare (_,n1) (_,n2) = compare n1 n2
  end
  )

type 'a table = 'a Table.t

(* 型のテーブル *)
let empty = Table.empty

(* 新しい束縛を追加（古い表を更新せず、新しい表を生成する *)
let enter = Table.add

(* 表tableの中で記号keyの束縛bを見つけ、Some(b)を返す(keyの束縛が存在しないならNone) *)
let look key table =
  try Some (Table.find key table)
  with Not_found -> None
