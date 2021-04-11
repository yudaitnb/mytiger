open OUnit2
open Core.Types
open Util

(* リテラル用のテスト *)
let semantic_tests_for_literals =
  "semantic test for literals" >::: [
    test_utility_typechecker ("IntExp 1")
      ( "1" )
      ( INT );
    test_utility_typechecker ("IntExp 1234")
      ( "1234" )
      ( INT );
    test_utility_typechecker ("StringExp (\"aiueo\")")
      ( "\"hoge\"" )
      ( STRING );
  ]

let semantic_tests_for_binops_basics =
  "semantic test for binops" >::: [
    test_utility_typechecker ("1 + 1")
      ( "1 + 1" )
      ( INT )
      ;
    test_utility_typechecker ("1 - 1")
      ( "1 - 1" )
      ( INT )
      ;
    test_utility_typechecker ("1 * 1")
      ( "1 * 1" )
      ( INT )
      ;
    test_utility_typechecker ("1 / 1")
      ( "1 / 1" )
      ( INT )
      ;
    test_utility_typechecker ("1 = 1")
      ( "1 = 1" )
      ( INT )
      ;
    test_utility_typechecker ("1 <> 1")
      ( "1 <> 1" )
      ( INT )
      ;
    test_utility_typechecker ("1 < 1")
      ( "1 < 1" )
      ( INT )
      ;
    test_utility_typechecker ("1 <= 1")
      ( "1 <= 1" )
      ( INT )
      ;
    test_utility_typechecker ("1 > 1")
      ( "1 > 1" )
      ( INT )
      ;
    test_utility_typechecker ("1 >= 1")
      ( "1 >= 1" )
      ( INT )
      ;
  ]
  
let semantic_tests_for_binops_assoc =
  "semantic test for binops" >::: [
    test_utility_typechecker ("1 + 1 * 1")
      ( "1 + 1 * 1" )
      ( INT );
    test_utility_typechecker ("1 - 1 / 1")
      ( "1 - 1 / 1" )
      ( INT );
    test_utility_typechecker ("(1 + 1) * 1")
      ( "(1 + 1) * 1" )
      ( INT );
    test_utility_typechecker ("1 + 1 = 2")
      ( "1 + 1 = 2" )
      ( INT );
    test_utility_typechecker ("1 + 1 > 1")
      ( "1 + 1 > 1" )
      ( INT );
    test_utility_typechecker ("1 + 1 = 2 & 1")
      ( "1 + 1 = 2 & 1" )
      ( INT );
  ]

let semantic_test =
  run_test_tt_main semantic_tests_for_literals;
  run_test_tt_main semantic_tests_for_binops_basics;
  run_test_tt_main semantic_tests_for_binops_assoc;