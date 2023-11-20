(* base *)

type 't ol_id_with_t_base = { id : string; t : 't option }
[@@deriving show]
type 'e ol_match_branch_base = {
  id : string;
  vars : string list;
  e : 'e }
[@@deriving show]
type ('e, 't) ol_fun_base = { params : 't ol_id_with_t_base list; t : 't option; e : 'e }
[@@deriving show]
type 'e ol_appl_base = { f : 'e; a : 'e }
[@@deriving show]
type 'e ol_if_base = { cond : 'e; e_if : 'e; e_else : 'e }
[@@deriving show]
type 'e ol_match_base = { e : 'e; branches : 'e ol_match_branch_base list }
[@@deriving show]

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

(* stage 2 - expression post-transformation *)
type ol_expr_l2 =
  | LetExpr of { id : string; is_rec : bool; t : tc_type option; expr : ol_expr_l2; body : ol_expr_l2 }
  | FunExpr of (ol_expr_l2, tc_type) ol_fun_base
  | ApplExpr of ol_expr_l2 ol_appl_base
  | IfExpr of ol_expr_l2 ol_if_base
  | TupleExpr of ol_expr_l2 list
  | MatchExpr of ol_expr_l2 ol_match_base
  | IntExpr of int
  | BoolExpr of bool
  | StringExpr of string
  | UnitExpr
  | IdExpr of string
  | BuiltinFunExpr of (_context_entry list -> ol_val) [@printer fun fmt _ -> Format.pp_print_string fmt "<builtin function>"]

and tc_type =
  | TInt
  | TBool
  | TString
  | TUnit
  | TFun of tc_type * tc_type
  | TTuple of tc_type list
  | TId of string (* variant types *)
  | TVar of string
  | TForall of string * tc_type
[@@deriving show]

(* interpretation stuff *)

and ol_val =
  | IntVal of int
  | StringVal of string
  | BoolVal of bool
  | UnitVal
  | TupleVal of ol_val list
  | VariantVal of string * ol_val (* non-constant constructor applied to an argument *)
  | ConstructorVal of string (* constant constructor, or non-constant constructor that hasn't been applied yet - treated like a function *)
  | ClosureVal of { params : string list; expr : ol_expr_l2 _context_t; rec_symbol : string option; }
[@@deriving show { with_path = false }]

(* redefine manually to not have circular deps *)
(* these aren't constructors so we're good :thumbs-up: *)
and _context_entry = (string * ol_val)
and 'a _context_t = 'a * (_context_entry list)

(* need to have it in here..... *)
module IContext = struct
  include Context.Make (struct
    type t = (string * ol_val) [@@deriving show]
  end)
  let get (k : string) (a : 'a t) = let c = list a in List.assoc_opt k c
end

let id_of_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Mod -> "\\mod" (* \ is not a character users can use in ids *)
  | Lt -> "<"
  | Eq -> "="
  | Concat -> "^"
  | And -> "&&"
  | Or -> "||"

let id_of_unop = function
  | Not -> "\\not"
  | Negate -> "~"