open OUnit2
open Core.Ast
open Core.Util

let dummy_id = 0
let dummy_loc = Core.Location.dummy_loc

let lit_int_1 = IntExp {value = 1; loc = dummy_loc }
let lit_int_2 = IntExp {value = 2; loc = dummy_loc }
let lit_bool_true = BoolExp {value = true; loc = dummy_loc }
let lit_bool_false = BoolExp {value = false; loc = dummy_loc }

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