open Ast
open Grammar

(* exception ParseError of string *)

include Nice_parser.Make(struct
  type result = ol_prog
  type token = Grammar.token
  exception ParseError = Grammar.Error
  let parse = Grammar.prog
  include Lexer
end)

let () = pp_exceptions ()

let parse = parse_string

(* let parse (src : string) : ol_prog = *)
(*  src |> Lexing.from_string |> Grammar.prog Lexer.tok *)