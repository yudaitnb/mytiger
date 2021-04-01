open OUnit2
open Core.Ast
open Util

(* リテラル用のテスト *)
let parse_tests_for_literals =
  "parse test for literals" >::: [
    parse_test_exp ("Lit (Int 1)")
      ( "1" )
      ( Int {value = 1; pos = dummy_pos; } );
    parse_test_exp ("Lit (Bool false)")
      ( "1234" )
      ( Int {value = 1234; pos = dummy_pos; } );
    parse_test_exp ("Lit (Bool true)")
      ( "true" )
      ( Bool {value = true; pos = dummy_pos; } );
    parse_test_exp ("Lit (Bool false)")
      ( "false" )
      ( Bool {value = false; pos = dummy_pos; } );
    parse_test_exp ("Lit (String \"aiueo\")")
      ( "\"hoge\"" )
      ( String {value = "hoge"; pos = dummy_pos; } );
  ]

let parse_tests_for_binops_basics =
  "parse test for binops" >::: [
    parse_test_exp ("1 + 1")
      ( "1 + 1" )
      ( BinOp {op = Add; e1=lit_int_1; e2=lit_int_1; pos = dummy_pos;} )
      ;
    parse_test_exp ("1 - 1")
      ( "1 - 1" )
      ( BinOp {op = Sub; e1=lit_int_1; e2=lit_int_1; pos = dummy_pos;} )
      ;
    parse_test_exp ("1 * 1")
      ( "1 * 1" )
      ( BinOp {op = Mul; e1=lit_int_1; e2=lit_int_1; pos = dummy_pos;} )
      ;
    parse_test_exp ("1 / 1")
      ( "1 / 1" )
      ( BinOp {op = Div; e1=lit_int_1; e2=lit_int_1; pos = dummy_pos;} )
      ;
    parse_test_exp ("1 = 1")
      ( "1 = 1" )
      ( BinOp {op = Eq; e1=lit_int_1; e2=lit_int_1; pos = dummy_pos;} )
      ;
    parse_test_exp ("1 <> 1")
      ( "1 <> 1" )
      ( BinOp {op = Neq; e1=lit_int_1; e2=lit_int_1; pos = dummy_pos;} )
      ;
    parse_test_exp ("1 < 1")
      ( "1 < 1" )
      ( BinOp {op = Lt; e1=lit_int_1; e2=lit_int_1; pos = dummy_pos;} )
      ;
    parse_test_exp ("1 <= 1")
      ( "1 <= 1" )
      ( BinOp {op = Lte; e1=lit_int_1; e2=lit_int_1; pos = dummy_pos;} )
      ;
    parse_test_exp ("1 > 1")
      ( "1 > 1" )
      ( BinOp {op = Gt; e1=lit_int_1; e2=lit_int_1; pos = dummy_pos;} )
      ;
    parse_test_exp ("1 >= 1")
      ( "1 >= 1" )
      ( BinOp {op = Gte; e1=lit_int_1; e2=lit_int_1; pos = dummy_pos;} )
      ;
    parse_test_exp ("true & true")
      ( "true & true" )
      ( BinOp {op = And; e1=lit_bool_true; e2=lit_bool_true; pos = dummy_pos;} )
      ;
    parse_test_exp ("true | false")
      ( "true | true" )
      ( BinOp {op = Or; e1=lit_bool_true; e2=lit_bool_true; pos = dummy_pos;} )
      ;
  ]

  let parse_tests_for_binops_assoc =
    "parse test for binops" >::: [
      parse_test_exp ("1 + 1 * 1")
        ( "1 + 1 * 1" )
        ( BinOp {
          op = Add;
          e1 = lit_int_1;
          e2 = BinOp {
            op = Mul;
            e1 = lit_int_1;
            e2 = lit_int_1;
            pos = dummy_pos; };
          pos = dummy_pos; } );
      parse_test_exp ("1 - 1 / 1")
        ( "1 - 1 / 1" )
        ( BinOp {
          op = Sub;
          e1 = lit_int_1;
          e2 = BinOp {
            op = Div;
            e1 = lit_int_1;
            e2 = lit_int_1;
            pos = dummy_pos; };
          pos = dummy_pos; } );
      parse_test_exp ("(1 + 1) * 1")
        ( "(1 + 1) * 1" )
        ( BinOp {
          op = Mul;
          e1 = SeqExp([
            BinOp {
              op = Add;
              e1 = lit_int_1;
              e2 = lit_int_1;
              pos = dummy_pos; }
          ]);
          e2 = lit_int_1;
          pos = dummy_pos; } );
      parse_test_exp ("1 + 1 = 2")
        ( "1 + 1 = 2" )
        ( BinOp {
          op = Eq;
          e1 = BinOp {
            op = Add;
            e1 = lit_int_1;
            e2 = lit_int_1;
            pos = dummy_pos; };
          e2 = lit_int_2;
          pos = dummy_pos; } );
      parse_test_exp ("1 + 1 > 1")
        ( "1 + 1 > 1" )
        ( BinOp {
          op = Gt;
          e1 = BinOp {
            op = Add;
            e1 = lit_int_1;
            e2 = lit_int_1;
            pos = dummy_pos; };
          e2 = lit_int_1;
          pos = dummy_pos; } );
      parse_test_exp ("1 + 1 = 2 & true")
        ( "1 + 1 = 2 & true" )
        ( BinOp {
          op = And;
          e1 = BinOp {
            op = Eq;
            e1 = BinOp {
              op = Add;
              e1 = lit_int_1;
              e2 = lit_int_1;
              pos = dummy_pos; };
            e2 = lit_int_2;
            pos = dummy_pos; };
          e2 = lit_bool_true;
          pos = dummy_pos; } );
    ]

let () =
  run_test_tt_main parse_tests_for_literals;
  run_test_tt_main parse_tests_for_binops_basics;
  run_test_tt_main parse_tests_for_binops_assoc;