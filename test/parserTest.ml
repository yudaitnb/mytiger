open OUnit2
open Core.Ast
open Util

(* リテラル用のテスト *)
let parse_tests_for_literals =
  "parse test for literals" >::: [
    test_utility_parser ("IntExp 0")
      ( "0" )
      ( IntExp { value = 0; loc = dummy_loc; } );
    test_utility_parser ("IntExp 1234")
      ( "1234" )
      ( IntExp { value = 1234; loc = dummy_loc; } );
    test_utility_parser ("StringExp (\"aiueo\")")
      ( "\"hoge\"" )
      ( StringExp { value = "hoge"; loc = dummy_loc; } );
    test_utility_parser "foo_"
      ( "\"foo_\"" )
      ( StringExp { value = "foo_"; loc = dummy_loc; });
    test_utility_parser "bar1"
      ( "\"bar1\"" )
      ( StringExp { value = "bar1"; loc = dummy_loc; });
    test_utility_parser ("nil")
      ( "nil" )
      ( NilExp { loc = dummy_loc; } );
  ]

let parse_tests_for_binops =
  "parse test for binops" >::: [
    test_utility_parser ("1 + 1")
      ( "1 + 1" )
      ( BinOpExp { op = Add; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; } );
    test_utility_parser ("1 - 1")
      ( "1 - 1" )
      ( BinOpExp { op = Sub; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; } );
    test_utility_parser ("1 * 1")
      ( "1 * 1" )
      ( BinOpExp { op = Mul; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; } );
    test_utility_parser ("1 / 1")
      ( "1 / 1" )
      ( BinOpExp { op = Div; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; } );
    test_utility_parser ("1 = 1")
      ( "1 = 1" )
      ( BinOpExp { op = Eq;  e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; } );
    test_utility_parser ("1 <> 1")
      ( "1 <> 1" )
      ( BinOpExp { op = Neq; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; } );
    test_utility_parser ("1 < 1")
      ( "1 < 1" )
      ( BinOpExp { op = Lt;  e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; } );
    test_utility_parser ("1 <= 1")
      ( "1 <= 1" )
      ( BinOpExp { op = Lte; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; } );
    test_utility_parser ("1 > 1")
      ( "1 > 1" )
      ( BinOpExp { op = Gt;  e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; } );
    test_utility_parser ("1 >= 1")
      ( "1 >= 1" )
      ( BinOpExp { op = Gte; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; } );
    test_utility_parser ("1 & 1")
      ( "1 & 1" )
      ( BinOpExp { op = And; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; } );
    test_utility_parser ("1 | 1")
      ( "1 | 1" )
      ( BinOpExp { op = Or;  e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; } );
    test_utility_parser ("1 + 1 * 1")
      ( "1 + 1 * 1" )
      ( BinOpExp {
        op = Add;
        e1 = lit_int_1;
        e2 = BinOpExp {
          op = Mul;
          e1 = lit_int_1;
          e2 = lit_int_1;
          loc = dummy_loc; };
        loc = dummy_loc; } );
    test_utility_parser ("1 - 1 / 1")
      ( "1 - 1 / 1" )
      ( BinOpExp {
        op = Sub;
        e1 = lit_int_1;
        e2 = BinOpExp {
          op = Div;
          e1 = lit_int_1;
          e2 = lit_int_1;
          loc = dummy_loc; };
        loc = dummy_loc; } );
    test_utility_parser ("(1 + 1) * 1")
      ( "(1 + 1) * 1" )
      ( BinOpExp {
        op = Mul;
        e1 = SeqExp([
          BinOpExp {
            op = Add;
            e1 = lit_int_1;
            e2 = lit_int_1;
            loc = dummy_loc; }
        ]);
        e2 = lit_int_1;
        loc = dummy_loc; } );
    test_utility_parser ("1 + 1 = 2")
      ( "1 + 1 = 2" )
      ( BinOpExp {
        op = Eq;
        e1 = BinOpExp {
          op = Add;
          e1 = lit_int_1;
          e2 = lit_int_1;
          loc = dummy_loc; };
        e2 = lit_int_2;
        loc = dummy_loc; } );
    test_utility_parser ("1 + 1 > 1")
      ( "1 + 1 > 1" )
      ( BinOpExp {
        op = Gt;
        e1 = BinOpExp {
          op = Add;
          e1 = lit_int_1;
          e2 = lit_int_1;
          loc = dummy_loc; };
        e2 = lit_int_1;
        loc = dummy_loc; } );
    test_utility_parser ("1 + 1 = 2 & 1")
      ( "1 + 1 = 2 & 1" )
      ( BinOpExp {
        op = And;
        e1 = BinOpExp {
          op = Eq;
          e1 = BinOpExp {
            op = Add;
            e1 = lit_int_1;
            e2 = lit_int_1;
            loc = dummy_loc; };
          e2 = lit_int_2;
          loc = dummy_loc; };
        e2 = lit_int_1;
        loc = dummy_loc; } );
  ]

let parse_tests_for_unops =
  "parse test for unops" >::: [
    test_utility_parser ("-0")
      ( "-0" )
      ( BinOpExp {
        op = Sub;
        e1 = IntExp { value = 0; loc=dummy_loc };
        e2 = IntExp { value = 0; loc=dummy_loc };
        loc = dummy_loc;
      });
    test_utility_parser ("-1")
      ( "-1" )
      ( BinOpExp {
        op = Sub;
        e1 = IntExp { value = 0; loc=dummy_loc };
        e2 = IntExp { value = 1; loc=dummy_loc };
        loc = dummy_loc;
      });
    test_utility_parser ("-1*-1+-1")
      ( "-1*-1+-1" )
      ( BinOpExp {
        op = Add;
        e1 = BinOpExp {
          op = Mul;
          e1 = lit_int_minus_1;
          e2 = lit_int_minus_1;
          loc = dummy_loc;
        };
        e2 = lit_int_minus_1;
        loc = dummy_loc;
      });
  ]

let parse_tests_for_sequencing =
  "parse tests for sequencing" >::: [
    test_utility_parser "(nil; nil)"
      ( "(nil; nil)" )
      ( SeqExp [ lit_nil; lit_nil; ] );
    test_utility_parser "(1; 1; 1; 1; 2)"
      ( "(1; 1; 1; 1; 2)" )
      ( SeqExp [ lit_int_1; lit_int_1; lit_int_1; lit_int_1; lit_int_2; ] );
  ]

let parse_tests_for_function_call =
  "parse tests for function call" >::: [
    test_utility_parser "hoge()"
      ( "hoge()" )
      ( CallExp { func = "hoge"; args = []; loc = dummy_loc } )
  ]


let parser_test =
  run_test_tt_main parse_tests_for_literals;
  run_test_tt_main parse_tests_for_binops;
  run_test_tt_main parse_tests_for_unops;
  run_test_tt_main parse_tests_for_sequencing;