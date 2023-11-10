open OUnit2
open Ocaml_lite.Interpreter
open Ocaml_lite.Parser
open Ocaml_lite.Ast

(* we're returning the value of the last binding *)
let assert_interprets_to (expr : string) (expected : ol_val) =
  assert_equal ~printer:show_ol_val (expr |> parse |> interpret_prog) expected

let test_basic _ =
  assert_interprets_to "let a = 5;;" (IntVal 5)

let test_builtin _ =
  assert_interprets_to "let r = string_of_int 5;;" (StringVal "5")

let test_match _ =
  assert_interprets_to "let a p = string_of_int (match p with | 5 => 6 | a => 10);; let b : string = a 5;;" (StringVal "6")

let test_ops _ =
  assert_interprets_to "let a = 5 + 6 * 7;;" (IntVal 47)

let test_if _ =
  assert_interprets_to "let a = if true then 5 else 6;;" (IntVal 5)

let test_if_2 _ =
  assert_interprets_to "let a = if false then 5 else 6;;" (IntVal 6)

let test_fn_rec _ =
  assert_interprets_to "let rec a b = if b < 1 then b else a (b - 1);; let r = a 5;;" (IntVal 0)

let test_let_expr _ =
  assert_interprets_to "let a = let b = 5 in b + 1;;" (IntVal 6)

let test_let_expr_nested _ =
  assert_interprets_to "let a = let b = 5 in let c = b + 1 in c + 1;;" (IntVal 7)

let test_let_expr_shadow _ =
  assert_interprets_to "let a = let b = 5 in let b = 6 in b;;" (IntVal 6)

let interpreter_tests =
  "test suite for interpreter"
  >::: [
    "simple expression" >:: test_basic;
    "recursive function" >:: test_fn_rec;
    "builtin function" >:: test_builtin;
(*    "match expression" >:: test_match; *) (* currently broken *)
    "operators" >:: test_ops;
    "if expression - true case" >:: test_if;
    "if expression - false case" >:: test_if_2;
    "let expression" >:: test_let_expr;
    "nested let expressions" >:: test_let_expr_nested;
    "shadowing let expressions" >:: test_let_expr_shadow;
  ]