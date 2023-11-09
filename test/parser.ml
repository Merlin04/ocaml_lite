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
          }; {
            id = "i";
            vars = [];
            e = IdExpr "j";
          }]
        }; }
      }]
    })

let fun_appl_assoc_test _ =
  p_assert_equal_expr
    "(fun a b => fun c d => e) f g"
    (ApplExpr {
      f = ApplExpr {
        f = FunExpr {
          params = [{ id = "a"; t = None }; { id = "b"; t = None }];
          t = None;
          e = FunExpr {
            params = [{ id = "c"; t = None }; { id = "d"; t = None }];
            t = None;
            e = IdExpr "e";
          };
        };
        a = IdExpr "f"
      };
      a = IdExpr "g";
    })

let test_parser_type_decl _ =
  p_assert_equal
    "type a = A | B of int;;"
    [TypeBinding {
      id = "a";
      t = [
        { id = "A"; t = None };
        { id = "B"; t = Some IntType }
      ];
    }]

let test_parser_nested_unop _ =
  p_assert_equal_expr
    "not ~ not not a"
    (UnopExpr {
      op = Not;
      e = UnopExpr {
        op = Negate;
        e = UnopExpr {
          op = Not;
          e = UnopExpr {
            op = Not;
            e = IdExpr "a";
          }
        }
      }
    })

let test_parser_arith_assoc _ =
  p_assert_equal_expr
    "a + b * c"
    (BinopExpr {
      a = IdExpr "a";
      op = Plus;
      b = BinopExpr {
        a = IdExpr "b";
        op = Times;
        b = IdExpr "c";
      }
    })

let test_parser_unit_expr _ =
  p_assert_equal_expr
    "()"
    (UnitExpr)

let test_appl_in_tuple _ =
  p_assert_equal_expr
    "(a b, c, d e)"
    (TupleExpr [
      ApplExpr {
        f = IdExpr "a";
        a = IdExpr "b";
      };
      IdExpr "c";
      ApplExpr {
        f = IdExpr "d";
        a = IdExpr "e";
      }
    ])

let test_type_prec _ =
  p_assert_equal
    "type a = | A of int * unit * string -> string * int -> unit * int;;"
    [TypeBinding {
      id = "a";
      t = [{
        id = "A";
        t = Some (FunType (
          TupleType [IntType; UnitType; StringType],
          FunType (
            TupleType [StringType; IntType],
            TupleType [UnitType; IntType]
          )
        ))
      }]
    }]

let parse_tests =
  "test suite for parser"
  >::: [
    "basic let binding" >:: test_parser_let_binding;
    "let expression" >:: test_parser_let_expr;
    "nested match expressions" >:: test_parser_nested_match;
    "complex nested match expressions (and precedence of binops)" >:: test_parser_complex_nested_match;
    "fun and application associativity" >:: fun_appl_assoc_test;
    "type declaration" >:: test_parser_type_decl;
    "nested unops" >:: test_parser_nested_unop;
    "arith associativity" >:: test_parser_arith_assoc;
    "unit expression" >:: test_parser_unit_expr;
    "application in tuple" >:: test_appl_in_tuple;
    "tuple type precedence" >:: test_type_prec;
  ]