open OUnit2
open Ocaml_lite.Parser
open Ocaml_lite.Ast

let test_parser_let_binding _ =
  assert_equal
    (parse "let rec a (b : bool) c : bool = 5;;")
    ([LetBinding {
      id = "a";
      is_rec = true;
      params = [
        { id = "b"; t = Some BoolType; };
        { id = "c"; t = None; }
      ];
      t = Some BoolType;
      expr = IntExpr 5
    }])

let test_parser_let_expr _ =
  assert_equal
    (parse "let a = let b = 5 in c;;")
    ([LetBinding {
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
    }])

let test_parser_nested_match _ =
  assert_equal
    (parse "let a = match a with a => b | c => match d with e => f | g => h;;")
    ([LetBinding {
      id = "a";
      is_rec = false;
      params = [];
      t = None;
      expr = MatchExpr {
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
      }
    }])

let parse_tests =
  "test suite for parser"
  >::: [
    "basic let binding" >:: test_parser_let_binding;
    "let expression" >:: test_parser_let_expr;
    "nested match expressions" >:: test_parser_nested_match;
  ]