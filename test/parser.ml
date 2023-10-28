open OUnit2
open Ocaml_lite.Parser
open Ocaml_lite.Ast

 let p_assert_equal code expr = assert_equal (parse code) expr ~printer:show_ol_prog

let test_parser_let_binding _ =
  p_assert_equal
    "let rec a (b : bool) c : bool = 5;;"
    [LetBinding {
      id = "a";
      is_rec = true;
      params = [
        { id = "b"; t = Some BoolType; };
        { id = "c"; t = None; }
      ];
      t = Some BoolType;
      expr = IntExpr 5
    }]

let p_assert_equal_expr code expr =
  p_assert_equal ("let a = (" ^ code ^ ");;") [LetBinding {
    id = "a";
    is_rec = false;
    params = [];
    t = None;
    expr;
  }]

let test_parser_let_expr _ =
  p_assert_equal
    "let a = let b = 5 in c;;"
    [LetBinding {
      id = "a";
      is_rec = false;
      params = [];
      t = None;
      expr = LetExpr {
        l = {
          id = "b";
          is_rec = false;
          params = [];
          t = None;
          expr = IntExpr 5;
        };
        e = IdExpr "c"
      }
    }]

let test_parser_nested_match _ =
  p_assert_equal_expr
    "match a with a => b | c => match d with e => f | g => h"
    (MatchExpr {
      e = IdExpr "a";
      branches = [{
        id = "a";
        vars = [];
        e = IdExpr "b";
      }; {
        id = "c";
        vars = [];
        e = MatchExpr {
          e = IdExpr "d";
          branches = [{
            id = "e";
            vars = [];
            e = IdExpr "f";
          }; {
            id = "g";
            vars = [];
            e = IdExpr "h";
          }]
        }
      }]
    })

let test_parser_complex_nested_match _ =
  p_assert_equal_expr
    "match a with a => (match a with a => a) | c => not match d with e => f | g => h + 5 | i => j"
    (MatchExpr {
      e = IdExpr "a";
      branches = [{
        id = "a";
        vars = [];
        e = MatchExpr {
          e = IdExpr "a";
          branches = [{
            id = "a";
            vars = [];
            e = IdExpr "a"
          }]
        }
      }; {
        id = "c";
        vars = [];
        e = UnopExpr { op = Not; e = MatchExpr {
          e = IdExpr "d";
          branches = [{
            id = "e";
            vars = [];
            e = IdExpr "f";
          }; {
            id = "g";
            vars = [];
            e = BinopExpr {
              a = IdExpr "h";
              op = Plus;
              b = IntExpr 5;
            }
          }]
        }; }
      }; {
        id = "i";
        vars = [];
        e = IdExpr "j";
      }]
    })

let parse_tests =
  "test suite for parser"
  >::: [
    "basic let binding" >:: test_parser_let_binding;
    "let expression" >:: test_parser_let_expr;
    "nested match expressions" >:: test_parser_nested_match;
    "complex nested match expressions (and precedence of binops)" >:: test_parser_complex_nested_match
  ]