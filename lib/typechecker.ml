open Ast

let rec get_type_of : ol_expr -> (ol_type, string) result = function
  | IntExpr _ -> Ok IntType
  | BoolExpr _ -> Ok BoolType
  | StringExpr _ -> Ok StringType
  | UnitExpr -> Ok UnitType
  | _ -> Error "typechecking expression is unimplemented"

let is_well_typed e = e |> get_type_of |> Result.is_ok