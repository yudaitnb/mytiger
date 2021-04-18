open OUnit2
open Core.Ast
open Util

let parse_tests_for_literals =
  "parsing tests for literals" >::: [
    (* 整数リテラル *)
    test_utility_parser ("0")
      ( "0" )
      ( IntExp { value = 0; loc = dummy_loc; } );
    test_utility_parser ("1234")
      ( "1234" )
      ( IntExp { value = 1234; loc = dummy_loc; } );

    (* 整形文字リテラル *)
    test_utility_parser ("newline")
      ( {|"\n"|} )
      ( StringExp { value = "\n"; loc = dummy_loc; } );
    test_utility_parser ("tab")
      ( {|"\t"|} )
      ( StringExp { value = "\t"; loc = dummy_loc; } );

    (* 文字列リテラル *)
    test_utility_parser ("no characters")
      ( {|""|} )
      ( StringExp { value = ""; loc = dummy_loc; } );
    test_utility_parser ("whitespace")
      ( {|" "|} )
      ( StringExp { value = " "; loc = dummy_loc; } );
    test_utility_parser ("str1")
      ( {|"foo"|} )
      ( StringExp { value = "foo"; loc = dummy_loc; } );
    test_utility_parser ("str2")
      ( {|"xx1"|} )
      ( StringExp { value = "xx1"; loc = dummy_loc; } );
    test_utility_parser ("str3")
      ( {|"xx_xs1"|} )
      ( StringExp { value = "xx_xs1"; loc = dummy_loc; } );

    (* 10進ASCIIコードを含む文字列リテラル *)
    test_utility_parser ("\"\\065\"")
      ( {|"\065"|} )
      ( StringExp { value = "A"; loc = dummy_loc; } );
    test_utility_parser ("\"\\064\"")
      ( {|"\064"|} )
      ( StringExp { value = "@"; loc = dummy_loc; } );
    test_utility_parser ("\"\\126\"")
      ( {|"\126"|} )
      ( StringExp { value = "~"; loc = dummy_loc; } );

    (* 特殊文字リテラル *)
    test_utility_parser ("double quote")
      ( {|"\""|} )
      ( StringExp { value = "\""; loc = dummy_loc; } );
    test_utility_parser ("backslash")
      ( {| "\\" |} )
      ( StringExp { value = "\\"; loc = dummy_loc; } );

    (* 行跨ぎの文字列リテラル *)
    test_utility_parser "string straddles multiple lines"
      ( "\"ai\\ \n   \\u\\   \t  \\eo\"" )
      ( StringExp { value = "aiueo"; loc = dummy_loc; } );
  ]

let parse_tests_for_binops =
  "parsing tests for binops" >::: [
    (* 二項演算 *)
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

    (* 演算子の優先順位 *)
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
        e1 = BinOpExp {
            op = Add;
            e1 = lit_int_1;
            e2 = lit_int_1;
            loc = dummy_loc; };
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
    test_utility_parser ("1 < 2 | x > 1")
      ( "1 < 2 | x >= 1" )
      ( BinOpExp {
        op = Or;
        e1 = BinOpExp {
          op = Lt;
          e1 = lit_int_1;
          e2 = lit_int_2;
          loc = dummy_loc; };
        e2 = BinOpExp {
          op = Gte;
          e1 = varexp_x;
          e2 = lit_int_1;
          loc = dummy_loc; };
        loc = dummy_loc; } );
    test_utility_parser ("buffer=\" \" | buffer=\"\\n\"")
      ( "buffer=\" \" | buffer=\"\\n\"" )
      ( BinOpExp {
        op = Or;
        e1 = BinOpExp {
          op = Eq;
          e1 = VarExp {
            var = SimpleVar { name = "buffer"; loc = dummy_loc };
            loc = dummy_loc };
          e2 = StringExp { value = " "; loc = dummy_loc };
          loc = dummy_loc };
        e2 = BinOpExp {
          op = Eq;
          e1 = VarExp {
            var = SimpleVar { name = "buffer"; loc = dummy_loc };
            loc = dummy_loc };
          e2 = StringExp { value = "\n"; loc = dummy_loc };
          loc = dummy_loc };
        loc = dummy_loc; } );
  ]

let parse_tests_for_unops =
  "parsing tests for unops" >::: [
    (* 単項演算 *)
    test_utility_parser ("-0")
      ( "-0" )
      ( BinOpExp {
        op = Sub;
        e1 = lit_int_0;
        e2 = lit_int_0;
        loc = dummy_loc;
      } );
    test_utility_parser ("-1")
      ( "-1" )
      ( BinOpExp {
        op = Sub;
        e1 = lit_int_0;
        e2 = lit_int_1;
        loc = dummy_loc;
      } );
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
      } );
  ]

let parse_tests_for_sequencing =
  "parsing testss for sequencing" >::: [
    test_utility_parser "(nil; nil)"
      ( "(nil; nil)" )
      ( SeqExp [ lit_nil; lit_nil; ] );
    test_utility_parser "(1; 1; 1; 1; 2)"
      ( "(1; 1; 1; 1; 2)" )
      ( SeqExp [ lit_int_1; lit_int_1; lit_int_1; lit_int_1; lit_int_2; ] );
  ]

let parse_tests_for_records =
  "parsing testss for records" >::: [
    test_utility_parser ("nil")
      ( "nil" )
      ( NilExp { loc = dummy_loc; } );
    test_utility_parser "rcd { x = 1 }"
      ( "rcd { x = 1 }" )
      ( RecordExp {
        record_name = "rcd";
        record_fields =
          [ ("x", lit_int_1 ) ];
        loc = dummy_loc; } );
    test_utility_parser "person { name = \"bob\" }"
      ( "person { name = \"bob\" }" )
      ( RecordExp {
        record_name = "person";
        record_fields =
          [ ("name", StringExp { value = "bob"; loc = dummy_loc } ) ];
        loc = dummy_loc; } );
    test_utility_parser "rcd { x = 1, y = 2 }"
      ( "rcd { x = 1, y = 2 }" )
      ( RecordExp {
        record_name = "rcd";
        record_fields = [
          ("x", lit_int_1 );
          ("y", lit_int_2 ) ];
        loc = dummy_loc; } ); 
  ]

let parse_tests_for_arrays = 
  "parsing testss for arrays" >::: [
    test_utility_parser "intArray[1] of 0"
      ( "intArray[1] of 0" )
      ( ArrayExp {
        array_name ="intArray";
        size = lit_int_1;
        init = lit_int_0;
        loc = dummy_loc; } );
    test_utility_parser "a_b_c_1__2_3[1] of 0"
      ( "a_b_c_1__2_3[1] of 0" )
      ( ArrayExp {
        array_name ="a_b_c_1__2_3";
        size = lit_int_1;
        init = lit_int_0;
        loc = dummy_loc; } );
  ]

let parse_tests_for_assignments =
  "parsing testss for assignments" >::: [
    test_utility_parser "a := 100"
      ( "a := 1" )
      ( AssignExp {
        var = SimpleVar { name = "a"; loc = dummy_loc };
        exp = lit_int_1;
        loc = dummy_loc; } );
    test_utility_parser "arr[2] := 1"
      ( "arr[2] := 1" )
      ( AssignExp {
        var = SubscriptVar {
          var = SimpleVar { name = "arr"; loc = dummy_loc };
          exp = lit_int_2;
          loc = dummy_loc;
        };
        exp = lit_int_1;
        loc = dummy_loc; } );
    test_utility_parser "rcd.x := 1"
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

let parse_tests_for_if_expressions =
  "parsing testss for if expressions" >::: [
    test_utility_parser "if 1 then 1 else 1"
      ( "if 1 then 1 else 1" )
      ( IfExp {
        cond = lit_int_1;
        th = lit_int_1;
        el = Some (lit_int_1);
        loc = dummy_loc; } );
    test_utility_parser "if 1 then 1"
      ( "if 1 then 1" )
      ( IfExp {
        cond = lit_int_1;
        th = lit_int_1;
        el = None;
        loc = dummy_loc; } );
    test_utility_parser "if 1 + 1 < 2 then 1 * 2 else 1 * 2"
      ( "if 1 + 1 < 2 then 1 * 1 else 1 * 1" )
      ( IfExp {
        cond = BinOpExp {
          op = Lt;
          e1 = BinOpExp {
            op = Add;
            e1 = lit_int_1;
            e2 = lit_int_1;
            loc = dummy_loc; };
          e2 = lit_int_2;
          loc = dummy_loc; };
        th = BinOpExp { op = Mul; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; };
        el = Some (BinOpExp { op = Mul; e1=lit_int_1; e2=lit_int_1; loc = dummy_loc; });
        loc = dummy_loc; } );
    test_utility_parser "if 1 then if 1 then 1 else 1"
      ( "if 1 then if 1 then 1 else 1" )
      ( IfExp {
        cond = lit_int_1;
        th = IfExp {
          cond = lit_int_1;
          th = lit_int_1;
          el = Some (lit_int_1);
          loc = dummy_loc; };
        el = None;
        loc = dummy_loc; } );
  ]

let parse_tests_for_left_values =
  "parsing testss for left values" >::: [
    test_utility_parser "a"
      ( "a" )
      ( VarExp {
        var = SimpleVar { name = "a"; loc = dummy_loc };
        loc = dummy_loc } );
    test_utility_parser "rcd.x"
      ( "rcd.x" )
      ( VarExp {
        var = FieldVar {
          var = SimpleVar { name = "rcd"; loc = dummy_loc; };
          name = "x";
          loc = dummy_loc };
        loc = dummy_loc } );
    test_utility_parser "arr[2]"
      ( "arr[2]" )
      ( VarExp {
        var = SubscriptVar {
          var = SimpleVar { name = "arr"; loc = dummy_loc; };
          exp = lit_int_2;
          loc = dummy_loc };
        loc = dummy_loc } );
  ]

let parse_tests_for_loops = 
  "parsing testss for loops" >::: [
    test_utility_parser "break"
      ( "break" )
      ( BreakExp { loc = dummy_loc } );
    test_utility_parser "while 1 < 2 do 1"
      ( "while 1 < 2 do 1" )
      ( WhileExp {
        cond = BinOpExp { op = Lt; e1=lit_int_1; e2=lit_int_2; loc = dummy_loc; };
        body = lit_int_1;
        loc = dummy_loc;
      } );
    test_utility_parser "while 1 < 2 | x > 1 do 1"
      ( "while 1 < 2 | x >= 1 do 1" )
      ( WhileExp {
        cond = BinOpExp {
          op = Or;
          e1 = BinOpExp {
            op = Lt;
            e1 = lit_int_1;
            e2 = lit_int_2;
            loc = dummy_loc; };
          e2 = BinOpExp {
            op = Gte;
            e1 = varexp_x;
            e2 = lit_int_1;
            loc = dummy_loc; };
          loc = dummy_loc; };
        body = lit_int_1;
        loc = dummy_loc;
      } );
    test_utility_parser "for x := 1 to 2 do 1"
      ( "for x := 1 to 2 do 1" )
      ( ForExp {
        var = "x";
        lo = lit_int_1;
        hi = lit_int_2;
        body = lit_int_1;
        loc = dummy_loc
      } );
    test_utility_parser "nested-forloop"
      ( "
        for i := 0 to N-1 do
          for j := 0 to N-1 do
            1
      " )
      ( ForExp {
        var = "i";
        lo = lit_int_0;
        hi = varexp_N_minus_1;
        body = ForExp {
          var = "j";
          lo = lit_int_0;
          hi = varexp_N_minus_1;
          body = lit_int_1;
          loc = dummy_loc;
        };
        loc = dummy_loc;
      } );
    test_utility_parser "nested-whileloop"
      ( "
        while 1 < 2 do
          while 1 < 2 do
            1
      " )
      ( WhileExp {
        cond = BinOpExp { op = Lt; e1=lit_int_1; e2=lit_int_2; loc = dummy_loc; };
        body = WhileExp {
          cond = BinOpExp { op = Lt; e1=lit_int_1; e2=lit_int_2; loc = dummy_loc; };
          body = lit_int_1;
          loc = dummy_loc;
        };
        loc = dummy_loc;
      } );
    test_utility_parser "nested-loop with break"
      ( "
        for x := 0 to N-1 do
          while x <> 2 do
            break
      " )
      ( ForExp {
        var = "x";
        lo = lit_int_0;
        hi = varexp_N_minus_1;
        body = WhileExp {
          cond = BinOpExp { op = Neq; e1=varexp_x; e2=lit_int_2; loc = dummy_loc; };
          body = BreakExp { loc = dummy_loc };
          loc = dummy_loc;
        };
        loc = dummy_loc;
      } );
  ]

let parse_tests_for_function_call =
  "parsing testss for function call" >::: [
    test_utility_parser "hoge()"
      ( "hoge()" )
      ( CallExp { func = "hoge"; args = []; loc = dummy_loc } );
    test_utility_parser "foo_()"
      ( "foo_()" )
      ( CallExp { func = "foo_"; args = []; loc = dummy_loc } );
    test_utility_parser "foo1_bar2()"
      ( "foo1_bar2()" )
      ( CallExp { func = "foo1_bar2"; args = []; loc = dummy_loc } );
    test_utility_parser "foo(a)"
      ( "foo(a)" )
      ( CallExp {
        func = "foo";
        args = [
          VarExp {
            var = SimpleVar { name = "a"; loc = dummy_loc };
            loc = dummy_loc } ];
        loc = dummy_loc } );
    test_utility_parser "foo(a, 1, b)"
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

let parse_tests_for_let_expressions = 
  "parsing testss for let-expressions" >::: [
    test_utility_parser "let var x = 1 in 1 end"
      ( "let var x := 1 in 1 end" )
      ( LetExp {
        decs = [
          VarDec {
            var_name = "x";
            var_type = None;
            init_val = lit_int_1;
            loc = dummy_loc; }
        ];
        body = lit_int_1;
        loc = dummy_loc;
      } );
    test_utility_parser "let var x = 1 in 1;1 end"
      ( "let var x := 1 in 1;1 end" )
      ( LetExp {
        decs = [
          VarDec {
            var_name = "x";
            var_type = None;
            init_val = lit_int_1;
            loc = dummy_loc; }
        ];
        body = SeqExp [ lit_int_1; lit_int_1; ];
        loc = dummy_loc;
      } );
  test_utility_parser "let var x : int = 1 in 1 end"
    ( "let var x : int := 1 in 1 end" )
    ( LetExp {
      decs = [
        VarDec {
          var_name = "x";
          var_type = Some "int";
          init_val = lit_int_1;
          loc = dummy_loc; }
      ];
      body = lit_int_1;
      loc = dummy_loc;
    } );
  test_utility_parser "let type intArray = int in 1 end"
    ( "let type x = int in 1 end" )
    ( LetExp {
      decs = [
        TypeDec [
          { tyname = "x";
            ty = NameTy ("int", dummy_loc);
            tyloc = dummy_loc }
        ]
      ];
      body = lit_int_1;
      loc = dummy_loc;
    } );
  test_utility_parser "let type intArray = array of int in 1 end"
    ( "let type intArray = array of int in 1 end" )
    ( LetExp {
      decs = [
        TypeDec [
          { tyname = "intArray";
            ty = ArrayTy ("int", dummy_loc);
            tyloc = dummy_loc }
        ]
      ];
      body = lit_int_1;
      loc = dummy_loc;
    } );
  test_utility_parser "let type any = { any : int } in 1 end"
    ( "let type any = { any : int } in 1 end" )
    ( LetExp {
      decs = [
        TypeDec [
          { tyname = "any";
            ty = RecordTy [ { field_name="any"; field_type="int" } ];
            tyloc = dummy_loc }
        ]
      ];
      body = lit_int_1;
      loc = dummy_loc;
    } );
  test_utility_parser "let type hogefuga = { hoge : int, fuga : int } in 1 end"
    ( "let type hogefuga = { hoge : int, fuga : int } in 1 end" )
    ( LetExp {
      decs = [
        TypeDec [
          { tyname = "hogefuga";
            ty = RecordTy [
              { field_name="hoge"; field_type="int" };
              { field_name="fuga"; field_type="int" } ];
            tyloc = dummy_loc }
        ]
      ];
      body = lit_int_1;
      loc = dummy_loc;
    } );
    test_utility_parser "let function f() = 1 in 1 end"
    ( "let function f() = 1 in 1 end" )
    ( LetExp {
      decs = [
        FunDec [
          { name = "f";
            params = [];
            result_type = None;
            body = lit_int_1;
            loc = dummy_loc; }
        ]
      ];
      body = lit_int_1;
      loc = dummy_loc;
    } );
  test_utility_parser "let function f() : int = 1 in 1 end"
    ( "let function f() : int = 1 in 1 end" )
    ( LetExp {
      decs = [
        FunDec [
          { name = "f";
            params = [];
            result_type = Some "int";
            body = lit_int_1;
            loc = dummy_loc; }
        ]
      ];
      body = lit_int_1;
      loc = dummy_loc;
    } );
  test_utility_parser "let function f(x:int, y:int) : int = 1 in 1 end"
    ( "let function f(x:int, y:int) : int = 1 in 1 end" )
    ( LetExp {
      decs = [
        FunDec [
          { name = "f";
            params = [
              { field_name = "x"; field_type = "int" };
              { field_name = "y"; field_type = "int" }
            ];
            result_type = Some "int";
            body = lit_int_1;
            loc = dummy_loc; }
        ]
      ];
      body = lit_int_1;
      loc = dummy_loc;
    } );
  test_utility_parser "let-exp including multiple funcion defs"
    ( "
      let
        function f(x:int, y:int) : int = 1
        function g(x:int, y:int) : int = 1
      in 1 end
    " )
    ( LetExp {
      decs = [
        FunDec [
          { name = "f";
            params = [
              { field_name = "x"; field_type = "int" };
              { field_name = "y"; field_type = "int" }
            ];
            result_type = Some "int";
            body = lit_int_1;
            loc = dummy_loc; };
          { name = "g";
            params = [
              { field_name = "x"; field_type = "int" };
              { field_name = "y"; field_type = "int" }
            ];
            result_type = Some "int";
            body = lit_int_1;
            loc = dummy_loc; }
        ];
      ];
      body = lit_int_1;
      loc = dummy_loc;
    } );
  test_utility_parser "let-exp including multiple type defs"
    ( "
      let
        type any = { any : int }
        type hogefuga = { hoge : int, fuga : int }
      in 1 end
    " )
    ( LetExp {
      decs = [
        TypeDec [
          { tyname = "any";
            ty = RecordTy [
              { field_name="any"; field_type="int" } ];
            tyloc = dummy_loc };
          { tyname = "hogefuga";
            ty = RecordTy [
              { field_name="hoge"; field_type="int" };
              { field_name="fuga"; field_type="int" } ];
            tyloc = dummy_loc }
        ];
      ];
      body = lit_int_1;
      loc = dummy_loc;
    } );
  test_utility_parser "complicated let-exp 1"
    ( "
      let
        function f(x:int, y:int) : int = 1
        var x : int := 1
        type hogefuga = { hoge : int, fuga : int }
      in 1 end
    " )
    ( LetExp {
      decs = [
        FunDec [
          { name = "f";
            params = [
              { field_name = "x"; field_type = "int" };
              { field_name = "y"; field_type = "int" }
            ];
            result_type = Some "int";
            body = lit_int_1;
            loc = dummy_loc; }
        ];
        VarDec {
          var_name = "x";
          var_type = Some "int";
          init_val = lit_int_1;
          loc = dummy_loc; };
        TypeDec [
          { tyname = "hogefuga";
            ty = RecordTy [
              { field_name="hoge"; field_type="int" };
              { field_name="fuga"; field_type="int" } ];
            tyloc = dummy_loc }
        ]
      ];
      body = lit_int_1;
      loc = dummy_loc;
    } );
  test_utility_parser "a part of merge.tig 1"
    ( "
      let
        function isdigit(s : string) : int = 
          ord(buffer)>=ord(\"0\") & ord(buffer)<=ord(\"9\")
      in 1 end
    " )
    ( LetExp {
      decs = [
        FunDec [
          { name = "isdigit";
            params = [
              { field_name = "s"; field_type = "string" }
            ];
            result_type = Some "int";
            body = BinOpExp {
              op = And;
              e1 = BinOpExp {
                op = Gte;
                e1 = CallExp {
                  func = "ord";
                  args = [
                    VarExp {
                      var = SimpleVar {name = "buffer"; loc = dummy_loc };
                      loc = dummy_loc } ];
                  loc = dummy_loc };
                e2 = CallExp {
                  func = "ord";
                  args = [
                    StringExp {value = "0"; loc = dummy_loc } ];
                  loc = dummy_loc };
                loc = dummy_loc };
              e2 = BinOpExp {
                op = Lte;
                e1 = CallExp {
                  func = "ord";
                  args = [
                    VarExp {
                      var = SimpleVar {name = "buffer"; loc = dummy_loc };
                      loc = dummy_loc } ];
                  loc = dummy_loc };
                e2 = CallExp {
                  func = "ord";
                  args = [
                    StringExp {value = "9"; loc = dummy_loc } ];
                  loc = dummy_loc };
                loc = dummy_loc };
              loc = dummy_loc };
            loc = dummy_loc; }
        ];
      ];
      body = lit_int_1;
      loc = dummy_loc;
    } );
  ]

let list_of_parser_tests = [
  parse_tests_for_literals;
  parse_tests_for_binops;
  parse_tests_for_unops;
  parse_tests_for_sequencing;
  parse_tests_for_records;
  parse_tests_for_arrays;
  parse_tests_for_assignments;
  parse_tests_for_if_expressions;
  parse_tests_for_left_values;
  parse_tests_for_loops;
  parse_tests_for_function_call;
  parse_tests_for_let_expressions;
]

let parser_test =
  List.map run_test_tt_main list_of_parser_tests
