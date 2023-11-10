type ol_type =
  | FunType of ol_type * ol_type
  | TupleType of ol_type list
  | IntType
  | BoolType
  | StringType
  | UnitType
  | IdType of string
[@@deriving show]
type ol_binop = Plus | Minus | Times | Divide | Mod | Lt | Eq | Concat | And | Or
[@@deriving show]
type ol_unop = Not | Negate
[@@deriving show]
type ol_id_with_t = { id : string; t : ol_type option }
[@@deriving show]
type ol_let = {
  id : string;
  is_rec : bool;
  params : ol_id_with_t list;
  t : ol_type option;
  expr : ol_expr }
[@@deriving show]
and ol_binding =
  | TypeBinding of {
    id : string;
    t : ol_id_with_t list }
  | LetBinding of ol_let
[@@deriving show]
and ol_match_branch = {
 id : string;
 vars : string list;
 e : ol_expr }
[@@deriving show]
and ol_expr =
  | LetExpr of { l : ol_let; e : ol_expr }
  | FunExpr of { params : ol_id_with_t list; t : ol_type option; e : ol_expr }
  | ApplExpr of { f : ol_expr; a : ol_expr }
  | IfExpr of { cond : ol_expr; e_if : ol_expr; e_else : ol_expr }
  | TupleExpr of ol_expr list
  | BinopExpr of { a : ol_expr; op : ol_binop; b : ol_expr }
  | UnopExpr of { op : ol_unop; e : ol_expr }
  | MatchExpr of { e : ol_expr; branches : ol_match_branch list }
  | IntExpr of int
  | BoolExpr of bool
  | StringExpr of string
  | UnitExpr
  | IdExpr of string
  | BuiltinFunExpr of (_context_entry list -> ol_val) [@printer fun fmt _ -> Format.pp_print_string fmt "<builtin function>"]
[@@deriving show]

and ol_prog = ol_binding list
[@@deriving show]

and ol_val =
  | IntVal of int
  | StringVal of string
  | BoolVal of bool
  | UnitVal
  | TupleVal of ol_val list
  | VariantVal of string * ol_val (* non-constant constructor applied to an argument *)
  | ConstructorVal of string (* constant constructor, or non-constant constructor that hasn't been applied yet - treated like a function *)
  | ClosureVal of { params : string list; expr : ol_expr _context_t; rec_symbol : string option; } (*[@printer fun fmt _ -> Format.pp_print_string fmt funval_str]*)
[@@deriving show { with_path = false }]
(* fix weird circular dependency stuff - ideally this would go in context module but oh well *)
and _context_entry = (string * ol_val)
and 'a _context_t = 'a * (_context_entry list)