open OUnit2
open Ocaml_lite.Parser
open Ocaml_lite.Transform
open Ocaml_lite.Typechecker

(* placeholder until details of function are implemented *)
let assert_well_typed (expr : string) (expected : bool) =
  assert_equal (expr |> parse |> transform_prog |> is_well_typed) expected

let test_basic _ =
  assert_well_typed "let a = 5;;" true

let test_fn_app _ =
  assert_well_typed "let a = (fun (a : int) => a + 5) 6;;" true

let test_binding _ =
  assert_well_typed "let a = fun (a : int) => a + 5;; let b = a 6;;" true

let test_builtins_exist _ =
  assert_well_typed "let a : string = string_of_int 5;;" true

let test_builtins_type _ =
  assert_well_typed "let a : int = string_of_int 5;;" false

let test_op_typings _ =
  assert_well_typed "let a = a + ();;" false

let test_match _ =
  assert_well_typed "type list = Nil | Cons of int * list;; let a = Cons (5, Nil);; let _ = match a with | Nil => \"no\" | Cons (h, t) => (\"yes\" ^ (string_of_int h));;" true

let test_type_constructors _ =
  assert_well_typed "type a = | A | B of int;; let b : a = A;; let c : a = B 5;;" true

let test_type_constructors_check _ =
  assert_well_typed "type a = | A | B of int;; let b : a = B true;;" false

let typechecker_tests =
  "test suite for typechecker"
  >::: [
    "simple binding" >:: test_basic;
    "function application" >:: test_fn_app;
    "binding application" >:: test_binding;
    "builtin exists" >:: test_builtins_exist;
    "builtin strictly typed" >:: test_builtins_type;
    "operator typing" >:: test_op_typings;
    "match inference" >:: test_match;
    "type constructors" >:: test_type_constructors;
    "type constructors type-check properly" >:: test_type_constructors_check;
  ]