open OUnit2
open Core.Ast
open Util


(* リテラル用のテスト *)
let parse_tests_for_literals =
  "parse test for literals" >::: [
    parse_test_exp ("Lit (Int 1)") ( Lit (Int 1) ) ( "1" );
    parse_test_exp ("Lit (Bool false)") ( Lit (Int 1234) ) ( "1234" );
    parse_test_exp ("Lit (Bool true)") ( Lit (Bool true) ) ( "true" );
    parse_test_exp ("Lit (Bool false)") ( Lit (Bool false) ) ( "false" );
    parse_test_exp ("Lit (String \"aiueo\")") ( Lit (String "hoge") ) ( "\"hoge\"" );
  ]

let parse_tests_for_binops_basics =
  "parse test for binops" >::: [
    parse_test_exp ("1 + 1")
      ( BinOp {op = Add; e1=node_lit_1; e2=node_lit_1} )
      ( "1 + 1" );
    parse_test_exp ("1 - 1")
      ( BinOp {op = Sub; e1=node_lit_1; e2=node_lit_1} )
      ( "1 - 1" );
    parse_test_exp ("1 * 1")
      ( BinOp {op = Mul; e1=node_lit_1; e2=node_lit_1} )
      ( "1 * 1" );
    parse_test_exp ("1 / 1")
      ( BinOp {op = Div; e1=node_lit_1; e2=node_lit_1} )
      ( "1 / 1" );
    parse_test_exp ("1 = 1")
      ( BinOp {op = Eq; e1=node_lit_1; e2=node_lit_1} )
      ( "1 = 1" );
    parse_test_exp ("1 <> 1")
      ( BinOp {op = Neq; e1=node_lit_1; e2=node_lit_1} )
      ( "1 <> 1" );
    parse_test_exp ("1 < 1")
      ( BinOp {op = Lt; e1=node_lit_1; e2=node_lit_1} )
      ( "1 < 1" );
    parse_test_exp ("1 <= 1")
      ( BinOp {op = Lte; e1=node_lit_1; e2=node_lit_1} )
      ( "1 <= 1" );
    parse_test_exp ("1 > 1")
      ( BinOp {op = Gt; e1=node_lit_1; e2=node_lit_1} )
      ( "1 > 1" );
    parse_test_exp ("1 >= 1")
      ( BinOp {op = Gte; e1=node_lit_1; e2=node_lit_1} )
      ( "1 >= 1" );
    parse_test_exp ("true & true")
      ( BinOp {op = And; e1=node_lit_true; e2=node_lit_true} )
      ( "true & true" );
    parse_test_exp ("true | false")
      ( BinOp {op = Or; e1=node_lit_true; e2=node_lit_true} )
      ( "true | true" );
  ]

  let parse_tests_for_binops_assoc =
    "parse test for binops" >::: [
      parse_test_exp ("1 + 1 * 1")
        ( BinOp {
            op = Add;
            e1 = node_lit_1;
            e2 = gen_dummy_node_exp ( BinOp {
              op = Mul;
              e1 = node_lit_1;
              e2 = node_lit_1 }) } )
        ( "1 + 1 * 1" );
      parse_test_exp ("1 - 1 / 1")
        ( BinOp {
            op = Sub;
            e1 = node_lit_1;
            e2 = gen_dummy_node_exp ( BinOp {
              op = Div;
              e1 = node_lit_1;
              e2 = node_lit_1 }) } )
        ( "1 - 1 / 1" );
      parse_test_exp ("(1 + 1) * 1")
        ( BinOp {
          op = Mul;
          e1 = gen_dummy_node_exp ( SeqExp([
            gen_dummy_node_exp ( BinOp {
              op = Add;
              e1 = node_lit_1;
              e2 = node_lit_1 })
          ]));
          e2 = node_lit_1 } )
        ( "(1 + 1) * 1" );
      parse_test_exp ("1 + 1 = 2")
        ( BinOp {
          op = Eq;
          e1 = gen_dummy_node_exp ( BinOp {
            op = Add;
            e1 = node_lit_1;
            e2 = node_lit_1 });
          e2 = node_lit_2 } )
        ( "1 + 1 = 2" );
      parse_test_exp ("1 + 1 > 1")
        ( BinOp {
          op = Gt;
          e1 = gen_dummy_node_exp ( BinOp {
            op = Add;
            e1 = node_lit_1;
            e2 = node_lit_1 });
          e2 = node_lit_1 } )
        ( "1 + 1 > 1" );
      parse_test_exp ("1 + 1 = 2 & true")
        ( BinOp {
          op = And;
          e1 = gen_dummy_node_exp ( BinOp {
            op = Eq;
            e1 = gen_dummy_node_exp ( BinOp {
              op = Add;
              e1 = node_lit_1;
              e2 = node_lit_1 });
            e2 = node_lit_2 } );
          e2 = node_lit_true } )
        ( "1 + 1 = 2 & true" );
    ]

let () =
  run_test_tt_main parse_tests_for_literals;
  run_test_tt_main parse_tests_for_binops_basics;
  run_test_tt_main parse_tests_for_binops_assoc;