open OUnit2
open Core.Ast
open Util

(* リテラル用のテスト *)
let parse_tests_for_literals =
  "parse test for literals" >::: [
    parse_test_exp ("IntExp (1)")
      ( "1" )
      ( IntExp {value = 1; loc = dummy_loc; } );
    parse_test_exp ("BoolExp false")
      ( "1234" )
      ( IntExp {value = 1234; loc = dummy_loc; } );
    parse_test_exp ("BoolExp true")
      ( "true" )
      ( BoolExp {value = true; loc = dummy_loc; } );
    parse_test_exp ("BoolExp false")
      ( "false" )
      ( BoolExp {value = false; loc = dummy_loc; } );
    parse_test_exp ("StringExp (\"aiueo\")")
      ( "\"hoge\"" )
      ( StringExp {value = "hoge"; loc = dummy_loc; } );
  ]

let parse_tests_for_binops_basics =
  "parse test for binops" >::: [
    parse_test_exp ("1 + 1")
      ( "1 + 1" )
      ( BinOpExp {op = Add; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc;} )
      ;
    parse_test_exp ("1 - 1")
      ( "1 - 1" )
      ( BinOpExp {op = Sub; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc;} )
      ;
    parse_test_exp ("1 * 1")
      ( "1 * 1" )
      ( BinOpExp {op = Mul; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc;} )
      ;
    parse_test_exp ("1 / 1")
      ( "1 / 1" )
      ( BinOpExp {op = Div; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc;} )
      ;
    parse_test_exp ("1 = 1")
      ( "1 = 1" )
      ( BinOpExp {op = Eq; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc;} )
      ;
    parse_test_exp ("1 <> 1")
      ( "1 <> 1" )
      ( BinOpExp {op = Neq; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc;} )
      ;
    parse_test_exp ("1 < 1")
      ( "1 < 1" )
      ( BinOpExp {op = Lt; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc;} )
      ;
    parse_test_exp ("1 <= 1")
      ( "1 <= 1" )
      ( BinOpExp {op = Lte; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc;} )
      ;
    parse_test_exp ("1 > 1")
      ( "1 > 1" )
      ( BinOpExp {op = Gt; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc;} )
      ;
    parse_test_exp ("1 >= 1")
      ( "1 >= 1" )
      ( BinOpExp {op = Gte; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc;} )
      ;
    parse_test_exp ("true & true")
      ( "true & true" )
      ( BinOpExp {op = And; e1=lit_bool_true; e2=lit_bool_true; loc = dummy_loc;} )
      ;
    parse_test_exp ("true | false")
      ( "true | true" )
      ( BinOpExp {op = Or; e1=lit_bool_true; e2=lit_bool_true; loc = dummy_loc;} )
      ;
  ]

let parse_tests_for_binops_assoc =
  "parse test for binops" >::: [
    parse_test_exp ("1 + 1 * 1")
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
    parse_test_exp ("1 - 1 / 1")
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
    parse_test_exp ("(1 + 1) * 1")
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
    parse_test_exp ("1 + 1 = 2")
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
    parse_test_exp ("1 + 1 > 1")
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
    parse_test_exp ("1 + 1 = 2 & true")
      ( "1 + 1 = 2 & true" )
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
        e2 = lit_bool_true;
        loc = dummy_loc; } );
  ]

let parser_test =
  run_test_tt_main parse_tests_for_literals;
  run_test_tt_main parse_tests_for_binops_basics;
  run_test_tt_main parse_tests_for_binops_assoc;