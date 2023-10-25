type ol_type =
  | FunType of ol_type * ol_type
  | TupleType of ol_type * ol_type
  | IntType
  | BoolType
  | StringType
  | UnitType
  | IdType of string
type ol_binop = Plus | Minus | Times | Divide | Mod | Lt | Eq | Concat | And | Or
type ol_unop = Not | Negate
type ol_id_with_t = { id : string; t : ol_type option }
type ol_let = {
  id : string;
  is_rec : bool;
  params : ol_id_with_t list;
  t : ol_type option;
  expr : ol_expr }
and ol_binding =
  | TypeBinding of {
    id : string;
    t : ol_id_with_t list }
  | LetBinding of ol_let
and ol_match_branch = {
 id : string;
 vars : string list;
 e : ol_expr }
and ol_expr =
  | LetExpr of { l : ol_let; e : ol_expr }
  | IfExpr of { cond : ol_expr; e_if : ol_expr; e_else : ol_expr }
  | FunExpr of { params : ol_id_with_t list; t : ol_type option; e : ol_expr }
  | ApplExpr of { f : ol_expr; a : ol_expr }
  | TupleExpr of ol_expr list
  | BinopExpr of { a : ol_expr; op : ol_binop; b : ol_expr }
  | UnopExpr of { op : ol_unop; e : ol_expr }
  | MatchExpr of { e : ol_expr; branches : ol_match_branch list }
  | IntExpr of int
  | BoolExpr of bool
  | StringExpr of string
  | UnitExpr
  | IdExpr of string

type ol_prog = ol_binding list
