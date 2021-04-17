open OUnit2
open Util

let lexing_tests_for_reserved_keywords =
  "lexing tests for reserved keywords" >::: [
    test_utility_lexer "while"    "while"    WHILE;
    test_utility_lexer "for"      "for"      FOR;
    test_utility_lexer "to"       "to"       TO;
    test_utility_lexer "break"    "break"    BREAK;
    test_utility_lexer "let"      "let"      LET;
    test_utility_lexer "in"       "in"       IN;
    test_utility_lexer "end"      "end"      END;
    test_utility_lexer "function" "function" FUNCTION;
    test_utility_lexer "var"      "var"      VAR;
    test_utility_lexer "type"     "type"     TYPE;
    test_utility_lexer "array"    "array"    ARRAY;
    test_utility_lexer "if"       "if"       IF;
    test_utility_lexer "then"     "then"     THEN;
    test_utility_lexer "else"     "else"     ELSE;
    test_utility_lexer "do"       "do"       DO;
    test_utility_lexer "of"       "of"       OF;
    test_utility_lexer "of"       "nil"      NIL;
    test_utility_lexer "," "," COMMA;
    test_utility_lexer ":" ":" COLON;
    test_utility_lexer ";" ";" SEMICOLON;
    test_utility_lexer "(" "(" LPAREN;
    test_utility_lexer ")" ")" RPAREN;
    test_utility_lexer "[" "[" LBRACKET;
    test_utility_lexer "]" "]" RBRACKET;
    test_utility_lexer "{" "{" LBRACE;
    test_utility_lexer "}" "}" RBRACE;
    test_utility_lexer "." "." DOT;
    test_utility_lexer "+" "+" PLUS;
    test_utility_lexer "-" "-" MINUS;
    test_utility_lexer "*" "*" ASTERISK;
    test_utility_lexer "/" "/" SLASH;
    test_utility_lexer "<>" "<>" NEQ;
    test_utility_lexer "<"  "<"  LT;
    test_utility_lexer "<=" "<=" LTE;
    test_utility_lexer ">"  ">"  GT;
    test_utility_lexer ">=" ">=" GTE;
    test_utility_lexer "&"  "&"  AND;
    test_utility_lexer "|"  "|"  OR;
    test_utility_lexer ":=" ":=" ASSIGN;
    test_utility_lexer "INT1" "0" (INT 0);
    test_utility_lexer "INT2" "10" (INT 10);
    test_utility_lexer "INT3" "2384859" (INT 2384859);
    test_utility_lexer "id1" "foo" (ID "foo");
    test_utility_lexer "id2" "bar" (ID "bar");
    test_utility_lexer "id3" "hogehoge" (ID "hogehoge");
    test_utility_lexer "comment" "/*xxxxx*/" EOF;
    test_utility_lexer "comment-nested" "/*xxxxx /*yyy*/*/" EOF;
    test_utility_lexer "comment-nested-nested"
      "/*xxxxx /*yyy*/ /*zzzz*/ */ 1 /* zzz*/"
      (INT 1)
    ;
    ]

let lexing_tests_for_strings =
  "lexing tests for strings" >::: [
    (* 整形文字 *)
    test_utility_lexer "newline" "\"\\n\"" (STR "\n");
    test_utility_lexer "tab" "\"\\t\"" (STR "\t");

    (* 文字列 *)
    test_utility_lexer "no characters" "\"\"" (STR "");
    test_utility_lexer "whitespace" "\" \"" (STR " ");
    test_utility_lexer "str1" "\"foo\"" (STR "foo");
    test_utility_lexer "str2" "\"xx1\"" (STR "xx1");
    test_utility_lexer "str3" "\"xx_xs1\"" (STR "xx_xs1");

    (* 10進ASCIIコード -> 対応する文字 *)
    test_utility_lexer "\"\\065\"" "\"\\065\"" (STR "A");
    test_utility_lexer "\"\\065\"" "\"\\064\"" (STR "@");
    test_utility_lexer "\"\\126\"" "\"\\126\"" (STR "~");

    (* 特殊文字 *)
    test_utility_lexer "double quote" "\"\\\"\"" (STR "\"");
    test_utility_lexer "backslash" "\"\\\\\"" (STR "\\");

    (* 行跨ぎの文字列 *)
    test_utility_lexer "string straddles multiple lines"
      ( "\"ai\\ \n   \\u\\   \t  \\eo\"" )
      ( STR "aiueo" );
  ]

let list_of_lexer_tests = [
  lexing_tests_for_reserved_keywords;
  lexing_tests_for_strings
]

let lexer_test = List.map run_test_tt_main list_of_lexer_tests
