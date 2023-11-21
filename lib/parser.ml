open Ast_l1
open Grammar

include Nice_parser.Make(struct
  type result = ol_prog
  type token = Grammar.token
  exception ParseError = Grammar.Error
  let parse = Grammar.prog
  include Lexer
end)

let () = pp_exceptions ()

let parse = parse_string