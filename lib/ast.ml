type ol_prog = ol_binding list
type ol_type =
type ol_binding =
  | TypeBinding of string * ((string * ol_type) list)
  | LetBinding of string * bool