open Ast
open Grammar

exception ParseError of string

let parse (src : string) : ol_prog =
  src |> Lexing.from_string |> Grammar.prog Lexer.tok