open OUnit2
open Core.Ast
open Core.Util

let dummy_id = 0
let dummy_pos = (
  { pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 },
  { pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
)

let gen_dummy_node_exp exp =
  let res = {
    id = dummy_id;
    loc = dummy_pos;
    value = exp
  } in res

let gen_dummy_node_dec dec =
  let res = {
    id = dummy_id;
    loc = dummy_pos;
    value = dec
  } in res

let node_lit_0 = gen_dummy_node_exp (Lit (Int 0))
let node_lit_1 = gen_dummy_node_exp (Lit (Int 1))
let node_lit_2 = gen_dummy_node_exp (Lit (Int 2))
let node_lit_true = gen_dummy_node_exp (Lit (Bool true))
let node_lit_false = gen_dummy_node_exp (Lit (Bool false))

(* 
 * expノードの為のテスト
 * name     : テスト名
 * expected : 期待されるパース結果(exp型の値)
 * input    : プログラム(str型の値)
 *)
let parse_test_exp name expected input =
  name >::
  (fun _ -> assert_equal ~printer:(show_node pp_exp) ~cmp:(equal_node equal_exp)
    (gen_dummy_node_exp expected)
    (parse_from_string input)
  )