open Ast_base
(* stage 2 - expression post-transformation *)

type tc_type =
  | TInt
  | TBool
  | TString
  | TUnit
  | TFun of tc_type * tc_type
  | TTuple of tc_type list
  | TId of string (* variant types *)
  | TVar of int
[@@deriving show]

type tc_type_p = Mono of tc_type | Poly of (int list) * tc_type
[@@deriving show]

type ol_id_with_t_l2 = tc_type ol_id_with_t_base

type ol_expr_l2 =
  | LetExpr of { id : string; is_rec : bool; t : tc_type option; expr : ol_expr_l2; body : ol_expr_l2 }
  | TypeBindingExpr of { t : tc_type ol_type_binding_base; body : ol_expr_l2 }
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

and ol_match_branch_l2 = ol_expr_l2 ol_match_branch_base

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