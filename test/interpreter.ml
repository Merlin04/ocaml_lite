open OUnit2
open Ocaml_lite.Interpreter
open Ocaml_lite.Parser
open Ocaml_lite.Ast_l2
open Ocaml_lite.Transform

(* we're returning the value of the last binding *)
let assert_interprets_to (expr : string) (expected : ol_val) =
  assert_equal ~printer:show_ol_val expected (expr |> parse |> transform_prog |> interpret_prog_expr)

let test_basic _ =
  assert_interprets_to "let a = 5;;" (IntVal 5)

let test_builtin _ =
  assert_interprets_to "let r = string_of_int 5;;" (StringVal "5")

let test_match _ =
  assert_interprets_to "let a = match Test (5, 6) with Yeah => 6 | Test (b, c) => (b + 5, 8);;" (TupleVal [(IntVal 10); (IntVal 8)])

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

let test_recursive _ =
  assert_interprets_to "let rec a i = if i = 0 then 0 else a (i - 1);; let b = a 5;;" (IntVal 0)

let test_non_recursive_shadowing _ =
  assert_interprets_to "let a i = 6 + i;; let a i = a (6 + i);; let b = a 5;;" (IntVal 17)

let test_recursive_shadowing _ =
  assert_interprets_to "let a i = 6 + i;; let rec a i = if i = 0 then 0 else a (i - 1);; let b = a 5;;" (IntVal 0)

let test_tuples _ =
  assert_interprets_to "let f a = match a with C1 (a, b) => a - b | C2 (a, b, c) => a - b - c;; let _ = (f (C1 (6, 5)), f (C2 (6, 5, 2)));;" (TupleVal [(IntVal 1); (IntVal (-1))])

let test_recursion_multiple_args _ =
  assert_interprets_to "let rec f a b = if b = 0 then a else f (a + b) (b - 1);; let _ = f 5 4;;" (IntVal 15)

let test_first_class_variants _ = (* :) *)
  assert_interprets_to "let f b = (if b then A else B) 5;; let _ = (f true, f false);;" (TupleVal [(VariantVal ("A", IntVal 5)); (VariantVal ("B", IntVal 5))])

let test_constructors _ =
  assert_interprets_to "let _ = A;;" (ConstructorVal "A")

let interpreter_tests =
  "test suite for interpreter"
  >::: [
    "simple expression" >:: test_basic;
    "recursive function" >:: test_fn_rec;
    "builtin function" >:: test_builtin;
    "match expression" >:: test_match;
    "operators" >:: test_ops;
    "if expression - true case" >:: test_if;
    "if expression - false case" >:: test_if_2;
    "let expression" >:: test_let_expr;
    "nested let expressions" >:: test_let_expr_nested;
    "shadowing let expressions" >:: test_let_expr_shadow;
    "basic recursion" >:: test_recursive;
    "non-recursive shadowing" >:: test_non_recursive_shadowing;
    "recursive shadowing" >:: test_recursive_shadowing;
    "tuple order" >:: test_tuples;
    "first-class variants" >:: test_first_class_variants;
    "constructors" >:: test_constructors;
  ]