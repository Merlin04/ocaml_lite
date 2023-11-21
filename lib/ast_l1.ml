open Ast_base
(* stage 1 (parsing) *)

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
type ol_id_with_t = ol_type ol_id_with_t_base
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
and ol_match_branch = ol_expr ol_match_branch_base
[@@deriving show]
and ol_expr =
  | LetExpr of { l : ol_let; e : ol_expr }
  | FunExpr of (ol_expr, ol_type) ol_fun_base
  | ApplExpr of ol_expr ol_appl_base
  | IfExpr of ol_expr ol_if_base
  | TupleExpr of ol_expr list
  | BinopExpr of { a : ol_expr; op : ol_binop; b : ol_expr }
  | UnopExpr of { op : ol_unop; e : ol_expr }
  | MatchExpr of ol_expr ol_match_base
  | IntExpr of int
  | BoolExpr of bool
  | StringExpr of string
  | UnitExpr
  | IdExpr of string
[@@deriving show]
and ol_prog = ol_binding list
[@@deriving show]