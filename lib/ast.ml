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
[@@deriving show]

type ol_prog = ol_binding list
[@@deriving show]