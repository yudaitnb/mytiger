open OUnit2
open Core.Types
open Util

(* リテラル用のテスト *)
let semantic_tests_for_literals =
  "parse test for literals" >::: [
    semantic_test_exp ("IntExp (1)")
      ( "1" )
      ( INT );
    semantic_test_exp ("BoolExp false")
      ( "1234" )
      ( INT );
    semantic_test_exp ("BoolExp true")
      ( "true" )
      ( INT );
    semantic_test_exp ("BoolExp false")
      ( "false" )
      ( INT );
    semantic_test_exp ("StringExp (\"aiueo\")")
      ( "\"hoge\"" )
      ( STRING );
  ]

let semantic_test =
  run_test_tt_main semantic_tests_for_literals;