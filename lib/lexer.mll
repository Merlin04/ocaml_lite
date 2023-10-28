{

open Grammar

let tok_to_str (t : token) : string =
  match t with
  | Type -> "type"
  | Of -> "of"
  | Let -> "let"
  | Rec -> "rec"
  | In -> "in"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | Match -> "match"
  | With -> "with"
  | Fun -> "fun"
  | True -> "true"
  | False -> "false"
  | Mod -> "mod"
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TUnit -> "unit"
  | Eq -> "="
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Lt -> "<"
  | Concat -> "^"
  | And -> "&&"
  | Or -> "||"
  | Not -> "not"
  | Negate -> "~"
  | DoubleSemicolon -> ";;"
  | Colon -> ":"
  | Arrow -> "->"
  | DoubleArrow -> "=>"
  | LParen -> "("
  | RParen -> ")"
  | Pipe -> "|"
  | Comma -> ","
  | Id id -> id
  | Int i -> string_of_int i
  | String s -> "\"" ^ s ^ "\""
  | EOF -> "<eof>"

(** A (stateful) buffer for storing string constants. *)
let sb = Buffer.create 256

(** A new kind of error to be thrown when lexing fails. *)
exception LexError of string

}

(** Identifiers start with a letter or underscore than have any number of
    letters, underscores, or digits. *)
let id = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

let whitespace = [' ' '\t' '\r' '\n']+
let int = ['0'-'9']+

rule next_token = parse
| whitespace
    { next_token lexbuf }
| int as i
    { Int (int_of_string i) }
| "type"
    { Type }
| "of"
    { Of }
| "let"
    { Let }
| "rec"
    { Rec}
| "in"
    { In }
| "if"
    { If }
| "then"
    { Then }
| "else"
    { Else }
| "match"
    { Match }
| "with"
    { With }
| "fun"
    { Fun }
| "true"
    { True }
| "false"
    { False }
| "mod"
    { Mod }
| "int"
    { TInt }
| "bool"
    { TBool }
| "string"
    { TString }
| "unit"
    { TUnit }
| '='
    { Eq }
| '+'
    { Plus }
| '-'
    { Minus }
| '*'
    { Times }
| '/'
    { Divide }
| '<'
    { Lt }
| '^'
    { Concat }
| "not"
    { Not }
| '~'
    { Negate }
| "&&"
    { And }
| "||"
    { Or }
| ";;"
    { DoubleSemicolon }
| ':'
    { Colon }
| "->"
    { Arrow }
| "=>"
    { DoubleArrow }
| '('
    { LParen }
| ')'
    { RParen }
| '|'
    { Pipe }
| ','
    { Comma }
| '"'
    { Buffer.clear sb;
      string lexbuf;
      String (Buffer.contents sb) }
| "(*"
    { comment lexbuf }
| id as s
    { Id s }
| eof
    { EOF }
| _
    { raise (LexError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and string = parse
| '"'
    { () }
| _ as c
    { Buffer.add_char sb c;
      string lexbuf }
and comment = parse
| "*)"
    { next_token lexbuf }
| _
    { comment lexbuf }
