open OUnit2
open Core.Ast
open Core.Util

let dummy_id = 0
let dummy_pos = (
  { pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  (* , { pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } *)
)

let lit_int_1 = Int {value = 1; pos = dummy_pos }
let lit_int_2 = Int {value = 2; pos = dummy_pos }
let lit_bool_true = Bool {value = true; pos = dummy_pos }
let lit_bool_false = Bool {value = false; pos = dummy_pos }

(* 
 * expの為のparseテスト
 * name     : テスト名
 * expected : 期待されるパース結果(exp型の値)
 * input    : プログラム(str型の値)
 *)

let parse_test_exp name input expected =
  name >::
  (fun _ -> assert_equal ~printer:(show_exp) ~cmp:(equal_exp)
    (parse_from_string input)
    (expected)
  )