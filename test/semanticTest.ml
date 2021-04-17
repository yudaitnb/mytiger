open OUnit2
open Core.Types
open Util

(* リテラル用のテスト *)
let semantic_tests_for_literals =
  "semantic tests for literals" >::: [
    (* 整数リテラル *)
    test_utility_typechecker ("0")
      ( "0" )
      ( INT );
    test_utility_typechecker ("1234")
      ( "1234" )
      ( INT );

    (* 整形文字リテラル *)
    test_utility_typechecker ("newline")
      ( "\"\\n\"" )
      ( STRING );
    test_utility_typechecker ("tab")
      ( "\"\\t\"" )
      ( STRING );

    (* 文字列リテラル *)
    test_utility_typechecker ("no characters")
      ( "\"\"" )
      ( STRING );
    test_utility_typechecker ("whitespace")
      ( "\" \"" )
      ( STRING );
    test_utility_typechecker ("str1")
      ( "\"foo\"" )
      ( STRING );
    test_utility_typechecker ("str2")
      ( "\"xx1\"" )
      ( STRING );
    test_utility_typechecker ("str3")
      ( "\"xx_xs1\"" )
      ( STRING );

    (* 10進ASCIIコードを含む文字列リテラル *)
    test_utility_typechecker ("\"\\065\"")
      ( "\"\\065\"" )
      ( STRING );
    test_utility_typechecker ("\"\\065\"")
      ( "\"\\064\"" )
      ( STRING );
    test_utility_typechecker ("\"\\126\"")
      ( "\"\\126\"" )
      ( STRING );

    (* 特殊文字リテラル *)
    test_utility_typechecker ("double quote")
      ( "\"\\\"\"" )
      ( STRING );
    test_utility_typechecker ("backslash")
      ( "\"\\\\\"" )
      ( STRING );

    (* 行跨ぎの文字列リテラル *)
    test_utility_typechecker "string straddles multiple lines"
      ( "\"ai\\ \n   \\u\\   \t  \\eo\"" )
      ( STRING );
  ]

let semantic_tests_for_binops =
  "semantic tests for binops" >::: [
    (* 二項演算 *)
    test_utility_typechecker ("1 + 1")
      ( "1 + 1" )
      ( INT );
    test_utility_typechecker ("1 - 1")
      ( "1 - 1" )
      ( INT );
    test_utility_typechecker ("1 * 1")
      ( "1 * 1" )
      ( INT );
    test_utility_typechecker ("1 / 1")
      ( "1 / 1" )
      ( INT );
    test_utility_typechecker ("1 = 1")
      ( "1 = 1" )
      ( INT );
    test_utility_typechecker ("1 <> 1")
      ( "1 <> 1" )
      ( INT );
    test_utility_typechecker ("1 < 1")
      ( "1 < 1" )
      ( INT );
    test_utility_typechecker ("1 <= 1")
      ( "1 <= 1" )
      ( INT );
    test_utility_typechecker ("1 > 1")
      ( "1 > 1" )
      ( INT );
    test_utility_typechecker ("1 >= 1")
      ( "1 >= 1" )
      ( INT );
    test_utility_typechecker ("1 & 1")
      ( "1 & 1" )
      ( INT );
    test_utility_typechecker ("1 | 1")
      ( "1 | 1" )
      ( INT );

    (* 演算子の優先順位 *)
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

let semantic_tests_for_unops =
  "semantic tests for unops" >::: [
    (* 単項演算 *)
    test_utility_typechecker ("-0")
      ( "-0" )
      ( INT );
    test_utility_typechecker ("-1")
      ( "-1" )
      ( INT );
    test_utility_typechecker ("-1*-1+-1")
      ( "-1*-1+-1" )
      ( INT );
  ]

let semantic_tests_for_sequencing =
  "semantic tests for sequencing" >::: [
    test_utility_typechecker "(nil; nil)"
      ( "(nil; nil)" )
      ( NIL );
    test_utility_typechecker "(1; 1; 1; 1; 2)"
      ( "(1; 1; 1; 1; 2)" )
      ( INT );
  ]

(* let semantic_tests_for_records =
  "semantic tests for records" >::: [
    test_utility_typechecker ("nil")
      ( "nil" )
      ( NIL );
  ]

let semantic_tests_for_arrays = 
  "semantic tests for arrays" >::: [
    test_utility_typechecker "intArray[1] of 0"
      ( "intArray[1] of 0" )
      ( ArrayExp {
        array_name ="intArray";
        size = lit_int_1;
        init = lit_int_0;
        loc = dummy_loc; } );
    test_utility_typechecker "a_b_c_1__2_3[1] of 0"
      ( "a_b_c_1__2_3[1] of 0" )
      ( ArrayExp {
        array_name ="a_b_c_1__2_3";
        size = lit_int_1;
        init = lit_int_0;
        loc = dummy_loc; } );
  ] *)
(*
let semantic_tests_for_assignments =
  "semantic tests for assignments" >::: [
    test_utility_typechecker "a := 100"
      ( "a := 1" )
      ( AssignExp {
        var = SimpleVar { name = "a"; loc = dummy_loc };
        exp = lit_int_1;
        loc = dummy_loc; } );
    test_utility_typechecker "arr[2] := 1"
      ( "arr[2] := 1" )
      ( AssignExp {
        var = SubscriptVar {
          var = SimpleVar { name = "arr"; loc = dummy_loc };
          exp = lit_int_2;
          loc = dummy_loc;
        };
        exp = lit_int_1;
        loc = dummy_loc; } );
    test_utility_typechecker "rcd.x := 1"
      ( "rcd.x := 1" )
      ( AssignExp {
        var = FieldVar {
          var = SimpleVar { name = "rcd"; loc = dummy_loc };
          name = "x";
          loc = dummy_loc;
        };
        exp = lit_int_1;
        loc = dummy_loc; } );
  ]
*)
let semantic_tests_for_if_expressions =
  "semantic tests for if expressions" >::: [
    test_utility_typechecker "if 1 then 1 else 1"
      ( "if 1 then 1 else 1" )
      ( INT );
    (* test_utility_typechecker "if 1 then 1"
      ( "if 1 then 1" )
      ( IfExp {
        cond = lit_int_1;
        th = lit_int_1;
        el = None;
        loc = dummy_loc; } ); *)
    test_utility_typechecker "if 1 + 1 < 2 then 1 * 2 else 1 * 2"
      ( "if 1 + 1 < 2 then 1 * 1 else 1 * 1" )
      ( INT );
    (* test_utility_typechecker "if 1 then if 1 then 1 else 1"
      ( "if 1 then if 1 then 1 else 1" )
      ( IfExp {
        cond = lit_int_1;
        th = IfExp {
          cond = lit_int_1;
          th = lit_int_1;
          el = Some (lit_int_1);
          loc = dummy_loc; };
        el = None;
        loc = dummy_loc; } ); *)
  ]
(*
let semantic_tests_for_left_values =
  "semantic tests for left values" >::: [
    test_utility_typechecker "a"
      ( "a" )
      ( VarExp {
        var = SimpleVar { name = "a"; loc = dummy_loc };
        loc = dummy_loc } );
    test_utility_typechecker "rcd.x"
      ( "rcd.x" )
      ( VarExp {
        var = FieldVar {
          var = SimpleVar { name = "rcd"; loc = dummy_loc; };
          name = "x";
          loc = dummy_loc };
        loc = dummy_loc } );
    test_utility_typechecker "arr[2]"
      ( "arr[2]" )
      ( VarExp {
        var = SubscriptVar {
          var = SimpleVar { name = "arr"; loc = dummy_loc; };
          exp = lit_int_2;
          loc = dummy_loc };
        loc = dummy_loc } );
  ]
*)
let semantic_tests_for_loops = 
  "semantic tests for loops" >::: [
    test_utility_typechecker "break"
      ( "break" )
      ( UNIT );
    test_utility_typechecker "while 1 < 2 do 1"
      ( "while 1 < 2 do 1" )
      ( UNIT );
    test_utility_typechecker "while 1 < 2 | x > 1 do 1"
      ( "while 1 < 2 | x >= 1 do 1" )
      ( UNIT );
    test_utility_typechecker "for x := 1 to 2 do 1"
      ( "for x := 1 to 2 do 1" )
      ( UNIT );
    test_utility_typechecker "nested-forloop"
      ( "
        for i := 0 to N-1 do
          for j := 0 to N-1 do
            1
      " )
      ( UNIT );
    test_utility_typechecker "nested-whileloop"
      ( "
        while 1 < 2 do
          while 1 < 2 do
            1
      " )
      ( UNIT );
    test_utility_typechecker "nested-loop with break"
      ( "
        for x := 0 to N-1 do
          while x <> 2 do
            break
      " )
      ( UNIT );
  ]
(*
let semantic_tests_for_function_call =
  "semantic tests for function call" >::: [
    test_utility_typechecker "hoge()"
      ( "hoge()" )
      ( CallExp { func = "hoge"; args = []; loc = dummy_loc } );
    test_utility_typechecker "foo_()"
      ( "foo_()" )
      ( CallExp { func = "foo_"; args = []; loc = dummy_loc } );
    test_utility_typechecker "foo1_bar2()"
      ( "foo1_bar2()" )
      ( CallExp { func = "foo1_bar2"; args = []; loc = dummy_loc } );
    test_utility_typechecker "foo(a)"
      ( "foo(a)" )
      ( CallExp {
        func = "foo";
        args = [
          VarExp {
            var = SimpleVar { name = "a"; loc = dummy_loc };
            loc = dummy_loc } ];
        loc = dummy_loc } );
    test_utility_typechecker "foo(a, 1, b)"
      ( "foo(a, 1, b)" )
      ( CallExp {
        func = "foo";
        args = [
          VarExp {
            var = SimpleVar { name = "a"; loc = dummy_loc };
            loc = dummy_loc };
          IntExp {
            value = 1;
            loc = dummy_loc };
          VarExp {
            var = SimpleVar { name = "b"; loc = dummy_loc };
            loc = dummy_loc };
        ];
        loc = dummy_loc } )
    ]
*)
let semantic_tests_for_let_expressions = 
  "semantic tests for let-expressions" >::: [
    test_utility_typechecker "let var x = 1 in 1 end"
      ( "let var x := 1 in 1 end" )
      ( INT );
    test_utility_typechecker "let var x = 1 in 1;1 end"
      ( "let var x := 1 in 1;1 end" )
      ( INT );
  test_utility_typechecker "let var x : int = 1 in 1 end"
    ( "let var x : int := 1 in 1 end" )
    ( INT );
  test_utility_typechecker "let type intArray = int in 1 end"
    ( "let type x = int in 1 end" )
    ( INT );
  test_utility_typechecker "let type intArray = array of int in 1 end"
    ( "let type intArray = array of int in 1 end" )
    ( INT );
  test_utility_typechecker "let type any = { any : int } in 1 end"
    ( "let type any = { any : int } in 1 end" )
    ( INT );
  test_utility_typechecker "let type hogefuga = { hoge : int, fuga : int } in 1 end"
    ( "let type hogefuga = { hoge : int, fuga : int } in 1 end" )
    ( INT );
    test_utility_typechecker "let function f() = 1 in 1 end"
    ( "let function f() = 1 in 1 end" )
    ( INT );
  test_utility_typechecker "let function f() : int = 1 in 1 end"
    ( "let function f() : int = 1 in 1 end" )
    ( INT );
  test_utility_typechecker "let function f(x:int, y:int) : int = 1 in 1 end"
    ( "let function f(x:int, y:int) : int = 1 in 1 end" )
    ( INT );
  test_utility_typechecker "let-exp including multiple funcion defs"
    ( "
      let
        function f(x:int, y:int) : int = 1
        function g(x:int, y:int) : int = 1
      in 1 end
    " )
    ( INT );
  test_utility_typechecker "let-exp including multiple type defs"
    ( "
      let
        type any = { any : int }
        type hogefuga = { hoge : int, fuga : int }
      in 1 end
    " )
    ( INT );
  test_utility_typechecker "complicated let-exp 1"
    ( "
      let
        function f(x:int, y:int) : int = 1
        var x : int := 1
        type hogefuga = { hoge : int, fuga : int }
      in 1 end
    " )
    ( INT );
  ]


  let list_of_semantic_tests = [
    semantic_tests_for_literals;
    semantic_tests_for_binops;
    semantic_tests_for_unops;
    semantic_tests_for_sequencing;
    (* semantic_tests_for_records;
    semantic_tests_for_arrays;
    semantic_tests_for_assignments; *)
    semantic_tests_for_if_expressions;
    (* semantic_tests_for_left_values;
    semantic_tests_for_loops;
    semantic_tests_for_function_call; *)
    semantic_tests_for_let_expressions;
  ]

let semantic_test =
  List.map run_test_tt_main list_of_semantic_tests;
