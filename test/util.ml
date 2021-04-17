open OUnit2
open Core.Util
open Core.Lexer
open Core.Ast
module S = Core.Semantic
module T = Core.Types

let dummy_id = 0
let dummy_loc = Core.Location.dummy_loc

let lit_int_0 = IntExp { value = 0; loc = dummy_loc; }
let lit_int_1 = IntExp { value = 1; loc = dummy_loc; }
let lit_int_2 = IntExp { value = 2; loc = dummy_loc; }
let lit_nil = NilExp { loc = dummy_loc; }

let lit_int_minus_1 = BinOpExp {
  op = Sub;
  e1 = lit_int_0;
  e2 = lit_int_1;
  loc = dummy_loc;
}

let var_x = SimpleVar { name = "x"; loc = dummy_loc; }

let varexp_x = VarExp { var = var_x; loc = dummy_loc; }

let var_N = SimpleVar { name = "N"; loc = dummy_loc; }
let varexp_N = VarExp { var = var_N; loc = dummy_loc; }

let varexp_N_minus_1 =
  BinOpExp {
    op = Sub;
    e1 = varexp_N;
    e2 = lit_int_1;
    loc = dummy_loc; }

(* 
 * Lexerテストユーティリティ
 * name     : テスト名
 * input    : 入力プログラム(str型の値)
 * expected : 期待されるLexing値(token型の値)
 *)
let test_utility_lexer name input expected =
  let lexbuf = Lexing.from_string input in
  let tok = token lexbuf in
  name >::
  (fun _ -> assert_equal ~printer:(show_token) ~cmp:(equal_token)
    (tok)
    (expected)
  )

(* 
 * parserテストユーティリティ
 * name     : テスト名
 * input    : 入力プログラム(str型の値)
 * expected : 期待されるパース結果(exp型の値)
 *)
let test_utility_parser name input expected =
  name >::
  (fun _ -> assert_equal ~printer:(show_exp) ~cmp:(equal_exp)
    (expected)
    (parse_from_string input)
  )

(* 
 * type checkテストユーティリティ
 * name     : テスト名
 * input    : 入力プログラム(str型の値)
 * expected : 期待される型結果(exp型の値)
 *)
let test_utility_typechecker name input expected =
  name >::
  (fun _ -> assert_equal ~printer:(T.show_ty) ~cmp:(T.equal_ty)
    (expected)
    (S.typeof (parse_from_string input))
  )