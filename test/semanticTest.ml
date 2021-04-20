open OUnit2
open Core.Types
open Util
open Core.Error

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
    test_utility_typechecker_error ("1 < 2 | x > 1")
      ( "1 < 2 | x > 1" )
      ( Error "line 1, character 9: undefined variable x" );
      test_utility_typechecker_error ({|buffer=" " | buffer="\n"|})
      ( {|buffer=" " | buffer="\n"|} )
      ( Error "line 1, characters 1-6: undefined variable buffer" );
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

let semantic_tests_for_records =
  "semantic tests for records" >::: [
    test_utility_typechecker ("nil")
      ( "nil" )
      ( NIL );
    test_utility_typechecker_error "rcd { x = 1 }"
      ( "rcd { x = 1 }" )
      ( Error "line 1, characters 1-13: undefined type rcd" );
    test_utility_typechecker_error "person { name = \"bob\" }"
      ( "person { name = \"bob\" }" )
      ( Error "line 1, characters 1-23: undefined type person" );
    test_utility_typechecker_error "rcd { x = 1, y = 2 }"
      ( "rcd { x = 1, y = 2 }" )
      ( Error "line 1, characters 1-20: undefined type rcd" ); 
  ]

let semantic_tests_for_arrays = 
  "semantic tests for arrays" >::: [
    test_utility_typechecker_error "intArray[1] of 0"
      ( "intArray[1] of 0" )
      ( Error "line 1, characters 1-16: undefined type intArray" );
    test_utility_typechecker_error "a_b_c_1__2_3[1] of 0"
      ( "a_b_c_1__2_3[1] of 0" )
      ( Error "line 1, characters 1-20: undefined type a_b_c_1__2_3" );
  ]

let semantic_tests_for_assignments =
  "semantic tests for assignments" >::: [
    test_utility_typechecker_error "a := 100"
      ( "a := 1" )
      ( Error "line 1, character 1: undefined variable a" );
    test_utility_typechecker_error "arr[2] := 1"
      ( "arr[2] := 1" )
      ( Error "line 1, characters 1-6: undefined variable arr" );
    test_utility_typechecker_error "rcd.x := 1"
      ( "rcd.x := 1" )
      ( Error "line 1, characters 1-3: undefined variable rcd" );
  ]

let semantic_tests_for_if_expressions =
  "semantic tests for if expressions" >::: [
    test_utility_typechecker "if 1 then 1 else 1"
      ( "if 1 then 1 else 1" )
      ( INT );
    test_utility_typechecker_error "if 1 then 1"
      ( "if 1 then 1" )
      (* then節に対してerrorを告知したいが、現在はif-then節全体について告知している。 *)
      ( Error "line 1, characters 1-11: type mismatch: expected UNIT, but found INT" );
    test_utility_typechecker "if 1 + 1 < 2 then 1 * 2 else 1 * 2"
      ( "if 1 + 1 < 2 then 1 * 1 else 1 * 1" )
      ( INT );
    test_utility_typechecker_error "if 1 then if 1 then 1 else 1"
      ( "if 1 then if 1 then 1 else 1" )
      ( Error "line 1, characters 1-28: type mismatch: expected UNIT, but found INT" );
  ]

let semantic_tests_for_left_values =
  "semantic tests for left values" >::: [
    test_utility_typechecker_error "a"
      ( "a" )
      ( Error "line 1, character 1: undefined variable a" );
    test_utility_typechecker_error "rcd.x"
      ( "rcd.x" )
      ( Error "line 1, characters 1-3: undefined variable rcd" );
    test_utility_typechecker_error "arr[2]"
      ( "arr[2]" )
      ( Error "line 1, characters 1-6: undefined variable arr" );
  ]

let semantic_tests_for_loops = 
  "semantic tests for loops" >::: [
    test_utility_typechecker_error "break"
      ( "break" )
      ( Error "line 1, characters 1-5: breal exp isn't in loop exp" );
    test_utility_typechecker_error "while 1 < 2 do 1"
      ( "while 1 < 2 do 1" )
      ( Error ("line 1, characters 1-16: type mismatch: expected UNIT, but found INT"));
    test_utility_typechecker_error "while 1 < 2 | x > 1 do 1"
      ( "while 1 < 2 | x >= 1 do 1" )
      ( Error ("line 1, character 15: undefined variable x") );
    test_utility_typechecker_error "for x := 1 to 2 do 1"
      ( "for x := 1 to 2 do 1" )
      ( Error ("line 1, characters 1-20: type mismatch: expected UNIT, but found INT") );
    test_utility_typechecker_error "nested-forloop"
      ( "
        for i := 0 to N-1 do
          for j := 0 to N-1 do
            1
      " )
      ( Error "line 2, character 23: undefined variable N" );
    test_utility_typechecker_error "nested-whileloop"
      ( "
        while 1 < 2 do
          while 1 < 2 do
            1
      " )
      ( Error "line 3, characters 11-13: type mismatch: expected UNIT, but found INT" );
    test_utility_typechecker_error "nested-loop with break"
      ( "
        for x := 0 to N-1 do
          while x <> 2 do
            break
      " )
      ( Error "line 2, character 23: undefined variable N" );
  ]

let semantic_tests_for_function_call =
  "semantic tests for function call" >::: [
    test_utility_typechecker_error "hoge()"
      ( "hoge()" )
      ( Error "line 1, characters 1-6: undefined function hoge" );
    test_utility_typechecker_error "foo_()"
      ( "foo_()" )
      ( Error "line 1, characters 1-6: undefined function foo_" );
    test_utility_typechecker_error "foo1_bar2()"
      ( "foo1_bar2()" )
      ( Error "line 1, characters 1-11: undefined function foo1_bar2" );
    test_utility_typechecker_error "foo(a)"
      ( "foo(a)" )
      ( Error "line 1, characters 1-6: undefined function foo" );
    test_utility_typechecker_error "foo(a, 1, b)"
      ( "foo(a, 1, b)" )
      ( Error "line 1, characters 1-12: undefined function foo" )
    ]

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
    test_utility_typechecker_error "let function f() = 1 in 1 end"
      ( "let function f() = 1 in 1 end" )
      ( Error "line 1, characters 5-20: type mismatch: expected INT, but found UNIT" );
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

let semantic_tests_for_lvalue = 
  "semantic tests for let-expressions" >::: [
    test_utility_typechecker "array - valid"
      ( "
        let
          type intArray = array of int
          var row := intArray [5] of 0
        in
          1
        end
      " )
      ( INT );
    test_utility_typechecker "array - valid"
      ( "
        let
          var N := 8
          type intArray = array of int
          var row := intArray [N+N-1] of 0
        in
          1
        end
      " )
      ( INT );
    test_utility_typechecker_error "array - invalid initial element"
      ( {|
        let
          type intArray = array of int
          var row := intArray [5] of "0"
        in
          1
        end
      |} )
      ( Error ("line 4, characters 22-40: type mismatch: expected INT, but found STRING") );
    test_utility_typechecker_error "array - A number of element is undefined variable"
      ( {|
        let
          type intArray = array of int
          var row := intArray [N] of 0
        in
          1
        end
      |} )
      ( Error ("line 4, character 32: undefined variable N") );
    ]

let semantic_tests_for_function_declarations = 
  "semantic tests for function declarations" >::: [
    test_utility_typechecker "id-function for int"
      ( {|
      let
        function f(c:int):int = c
      in
        f(1)
      end
      |} )
      ( INT );
    test_utility_typechecker "id-function for string"
      ( {|
      let
        function f(s:string):string = s
      in
        f("s")
      end
      |} )
      ( STRING );
    test_utility_typechecker_error "invalid f argument"
      ( {|
      let
        function f(c:int):int = c
      in
        f("1")
      end
      |} )
      ( Error "line 5, characters 9-14: type mismatch: expected INT, but found STRING" );
    test_utility_typechecker_error "missing result type annotataion"
      ( {|
      let
        function f(c:int) = c
      in
        f(1)
      end
      |} )
      ( Error "line 3, characters 9-29: type mismatch: expected INT, but found UNIT" );
    test_utility_typechecker "muptiple arguments"
      ( {|
      let
        function plus(c:int,d:int):int = c + d
      in
        plus(1,2)
      end
      |} )
      ( INT );
    test_utility_typechecker "muptiple function definitions"
      ( {|
      let
        function plus(c:int,d:int):int = c + d
        function double(c:int):int = 2 * c
      in
        double(plus(1,2))
      end
      |} )
      ( INT );
    test_utility_typechecker "self-recursive functions"
      ( {|
      let
        function factorial(c:int):int =
          if c = 0 then 1 else c * factorial(c-1)
      in
        factorial(5)
      end
      |} )
      ( INT );
    test_utility_typechecker "mutual-recursive functions"
      ( {|
      let
        function even(c:int):int =
          if c=0 then 1 else (if c=1 then 0 else odd(c-1))
        function odd(c:int):int =
          if c=0 then 0 else (if c=1 then 1 else even(c-1))
      in
        odd(11)
      end
      |} )
      ( INT );
  ]

let semantic_tests_for_type_declarations = 
  "semantic tests for type declarations" >::: [
    test_utility_typechecker "single var declaration"
      ( {|
      let
        type a = int
      in
        1
      end
      |} )
      ( INT );
    test_utility_typechecker "multiple var declaration"
      ( {|
      let
        type a = int
        type b = string
      in
        1
      end
      |} )
      ( INT );
    test_utility_typechecker "record var declaration"
      ( {|
      let
        type a = { x:int, y:int }
      in
        1
      end
      |} )
      ( INT );
    test_utility_typechecker "record var declaration"
      ( {|
      let
        type a = { x:int, y:int }
      in
        1
      end
      |} )
      ( INT );
    test_utility_typechecker "recursive type definition"
      ( {|
        let
          type a = b
          type b = int
        in
          1
        end
      |} )
      ( INT );
    test_utility_typechecker_error "cyclic type definition 1"
      ( {|
        let
          type a = b
          type b = c
          type c = d
          type d = a
        in
          1
        end
      |} )
      (* TODO エラーメッセージおかしい *)
      ( Error "line 0, characters 0-0: d includes a cyclic type definition." );
    test_utility_typechecker_error "cyclic type definition 2"
      ( {|
        let
          type a = int
          type b = d
          type c = a
          type d = b
        in
          1
        end
      |} )
      (* TODO エラーメッセージおかしい *)
      ( Error "line 0, characters 0-0: d includes a cyclic type definition." );
    test_utility_typechecker "self-recursive record type definition"
      ( {|
        let
          type list = { first : int, rest : list }
          var x := list{first=0,rest=nil}
        in
          1
        end
      |} )
      ( INT );
    test_utility_typechecker "mutual-recursive record type definition"
      ( {|
        let
          type reca = { x : recb }
          type recb = { y : reca }
        in
          1
        end
      |} )
      ( INT );
  ]

let list_of_semantic_tests = [
  semantic_tests_for_literals;
  semantic_tests_for_binops;
  semantic_tests_for_unops;
  semantic_tests_for_sequencing;
  semantic_tests_for_records;
  semantic_tests_for_arrays;
  semantic_tests_for_assignments;
  semantic_tests_for_if_expressions;
  semantic_tests_for_left_values;
  semantic_tests_for_loops;
  semantic_tests_for_function_call;
  semantic_tests_for_let_expressions;
  semantic_tests_for_lvalue;
  semantic_tests_for_function_declarations;
  semantic_tests_for_type_declarations;
]

let semantic_test =
  List.map run_test_tt_main list_of_semantic_tests