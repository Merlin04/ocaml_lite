open OUnit2

(* placeholder until details of function are implemented *)
let assert_well_typed (expr : string) (expected : bool) =
  assert_equal (failwith "Typechecker is unimplemented") expected

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

let test_match_inference _ =
  assert_well_typed "let a p = string_of_int (match p with | 5 => 6 | a => 10);; let b : string = a 5;;" true

let typechecker_tests =
  "test suite for typechecker"
  >::: [
    "simple binding" >:: test_basic;
    "function application" >:: test_fn_app;
    "binding application" >:: test_binding;
    "builtin exists" >:: test_builtins_exist;
    "builtin strictly typed" >:: test_builtins_type;
    "operator typing" >:: test_op_typings;
    "match inference" >:: test_match_inference;
  ]