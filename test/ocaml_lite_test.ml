open OUnit2

let tests = "test suite for ocaml_lite" >::: [
    Lexer.lex_tests;
    Parser.parse_tests;
    (* Typechecker.typechecker_tests; *)
    Interpreter.interpreter_tests;
  ]

let _ = run_test_tt_main tests
