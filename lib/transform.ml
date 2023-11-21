open Ast_base
open Ast_l1
open Ast_l2

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

(* having separate tc and ol types makes things nicer in some places,
   but also means we have to do things like this *)
let rec tc_type_of_ol = function
  | FunType (f, a) -> TFun (tc_type_of_ol f, tc_type_of_ol a)
  | TupleType d -> TTuple (List.map tc_type_of_ol d)
  | IntType -> TInt
  | BoolType -> TBool
  | StringType -> TString
  | UnitType -> TUnit
  | IdType d -> TId d

let rec bindings_to_expr (p : ol_prog) : ol_expr =
  let rec help : ol_let list -> ol_expr = function
    | l :: t -> LetExpr { l; e = match t with [] -> IdExpr l.id | _ -> help t }
    | _ -> (* empty program *) UnitExpr in
  List.filter_map (function LetBinding l -> Some l | _ -> None) p |> help

and transform_match_branch (b : ol_match_branch) : ol_expr_l2 ol_match_branch_base = { id = b.id; vars = b.vars; e = transform_expr b.e }
and transform_id_with_t (i : ol_id_with_t) : ol_id_with_t_l2 = { i with t = Option.map tc_type_of_ol i.t }
and expr_of_ol_let (o : ol_let) : ol_expr_l2 = match o.params with
  | [] -> transform_expr o.expr
  | _ -> FunExpr { params = List.map transform_id_with_t o.params; t = Option.map tc_type_of_ol o.t; e = transform_expr o.expr }
and transform_expr : ol_expr -> ol_expr_l2 = function
  | BinopExpr { a; op; b } -> ApplExpr { f = ApplExpr { f = IdExpr (id_of_binop op); a = transform_expr a }; a = transform_expr b }
  | UnopExpr { op; e } -> ApplExpr { f = IdExpr (id_of_unop op); a = transform_expr e }
  | LetExpr { l = { id; is_rec; params; t; expr } as l; e } ->
    LetExpr { id; is_rec; t = Option.map tc_type_of_ol t; expr = expr_of_ol_let l ; body = transform_expr e }
  | FunExpr { params; t; e } ->
    FunExpr { params = List.map transform_id_with_t params; t = Option.map tc_type_of_ol t; e = transform_expr e }
  | ApplExpr { f; a } -> ApplExpr { f = transform_expr f; a = transform_expr a }
  | IfExpr { cond; e_if; e_else } -> IfExpr { cond = transform_expr cond; e_if = transform_expr e_if; e_else = transform_expr e_else }
  | TupleExpr l -> TupleExpr (List.map transform_expr l)
  | MatchExpr { e; branches } -> MatchExpr { e = transform_expr e; branches = List.map transform_match_branch branches }
  | IntExpr i -> IntExpr i
  | BoolExpr b -> BoolExpr b
  | StringExpr s -> StringExpr s
  | UnitExpr -> UnitExpr
  | IdExpr s -> IdExpr s

let rec transform_prog p = p |> bindings_to_expr |> transform_expr