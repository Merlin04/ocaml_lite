open Ast
open Tc_type
open Builtins

exception TCError of string

module Env = struct
  type t = (string * tc_type) list
  let get k (e : t) = List.assoc_opt k e |> function
    | Some v -> v
    | None -> raise (TCError ("Identifier " ^ k ^ " not found in environment"))
  let add_opt k v e = match v with
    | Some v -> (k, v) :: e
    | None -> e
end

module Constraints = struct
  include Context.Make (struct
    type t = tc_type * tc_type [@@deriving show]
  end)
  let add_opt k v (e : 'a t) = let- c = e in match v with
    | Some v -> (k, v) :: c
    | None -> c
end

open Constraints

let f_counter = ref 0
let fresh_var () = let v = !f_counter in f_counter := v + 1; TVar ("'" ^ string_of_int v)

let rec get_constraints (env : Env.t) : ol_expr -> tc_type Constraints.t =
  let get_c = get_constraints env in function
  | LetExpr { l; e } ->
    let { id; is_rec; expr } = l in
    let t_b = fresh_var () in
    let* t_val = get_constraints (env |> Env.add_opt id (if is_rec then Some t_b else None)) expr in
    let t_body = get_constraints ((id, t_b) :: env) e in t_body |> Constraints.add (t_b, t_val)
  | FunExpr { params; t; e } ->
    let (param, body) = (match params with
      | [p] -> (p, e)
      | p :: tail -> (p, FunExpr { params = tail; t; e; })
      | _ -> failwith "Received function with no params"
    ) in
    let t_arg = fresh_var () in
    let body_env = [(param.id, t_arg)] |> Env.add_opt param.id (Option.map tc_type_of_ol param.t) in
    let* t_body = get_constraints body_env body in
    Constraints.return (TFun (t_arg, t_body)) |> Constraints.add_opt t_body (Option.map tc_type_of_ol t)
  | ApplExpr { f; a } ->
    let t = fresh_var () in
    let* t_f = get_c f in
    let* t_a = get_c a in
    Constraints.make t [(t_f, TFun (t_a, t))]
  | IfExpr { cond; e_if; e_else } ->
    let* t_cond = get_c cond in
    let* t_if = get_c e_if in
    let* t_else = get_c e_else in
    let v = fresh_var () in
    Constraints.make v [(TBool, t_cond); (v, t_if); (v, t_else)]
  | TupleExpr l ->
    List.map get_c l |> List.fold_left (fun acc cur ->
      let* a_acc = acc in
      let+ t_cur = cur in
      t_cur :: a_acc
    ) (Constraints.return []) >|= (fun v -> TTuple v)
  | MatchExpr
  | v -> Constraints.return (match v with
    | IdExpr i -> env |> Env.get i
    | IntExpr _ -> TInt
    | StringExpr _ -> TString
    | BoolExpr _ -> TBool
    | UnitExpr -> TUnit
  )





let rec get_type_of : ol_expr -> (ol_type, string) result = function
  | IntExpr _ -> Ok IntType
  | BoolExpr _ -> Ok BoolType
  | StringExpr _ -> Ok StringType
  | UnitExpr -> Ok UnitType
  | _ -> Error "typechecking expression is unimplemented"

let is_well_typed e = e |> get_type_of |> Result.is_ok