open OUnit2

let funval_str = "<function>"

type ol_val =
  | IntVal of int
  | StringVal of string
  | BoolVal of bool
  | UnitVal
  | TupleVal of ol_val * ol_val
  | FunVal of (ol_val list) -> ol_val [@printer fun fmt _ -> Format.pp_print_string fmt funval_str]
[@@deriving show { with_path = false }]

(* placeholder until all functions are implemented *)
(* we'll assume we're returning the value of the last binding *)
let assert_interprets_to (expr : string) (expected : ol_val) =
  assert_equal ~printer:show_ol_val (failwith "Interpreter is unimplemented") expected

let test_basic _ =
  assert_interprets_to "let a = 5;;" (IntVal 5)

let test_fn_rec _ =
  assert_interprets_to "let rec a b = if b < 1 then b else a (b - 1);; let r = a 5;;" (IntVal 0)

let test_builtin _ =
  assert_interprets_to "let r = string_of_int 5;;" (StringVal "5")

let test_match _ =
  assert_interprets_to "let a p = string_of_int (match p with | 5 => 6 | a => 10);; let b : string = a 5;;" (StringVal "6")

let interpreter_tests =
  "test suite for interpreter"
  >::: [
    "simple expression" >:: test_basic;
    "recursive function" >:: test_fn_rec;
    "builtin function" >:: test_builtin;
    "match expression" >:: test_match;
  ]