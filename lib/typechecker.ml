open Ast_l2
open Builtins

exception TCError of string

module Env = struct
  type t = (string * tc_type_p) list
  let get k (e : t) = List.assoc_opt k e |> function
    | Some v -> v
    | None -> raise (TCError ("Identifier " ^ k ^ " not found in environment"))
  let add_opt k v e = match v with
    | Some v -> (k, v) :: e
    | None -> e
end

module Constraints = struct
  include Context.Make (struct
    type t = tc_type_p * tc_type_p [@@deriving show]
  end)
  let add_opt k v (e : 'a t) = let- c = e in match v with
    | Some v -> (k, v) :: c
    | None -> c
end

open Constraints

let mono_c (a, b) : Constraints.entry = (Mono a, Mono b)

let mono t = Mono t
let map_mono t = Option.map mono t

let unwrap_p = function
  | Mono t -> t
  | Poly _ -> failwith "Found polytype in invalid position"

let f_counter = ref 10
let fresh_var () = let v = !f_counter in f_counter := v + 1; TVar v

type tvar_mapping = int * tc_type

let rec occurs_in (i : int) : tc_type -> bool =
  let r = occurs_in i in function
    | TFun (a, b) -> r a || r b
    | TTuple l -> List.exists r l
    | TVar v -> v = i
    | TId _ | TInt | TBool | TString | TUnit -> false

let occurs_in_p (i : int) : tc_type_p -> bool = function
  | Poly (_, t) | Mono t -> occurs_in i t

(* mappings and constraints *)
type unify_result = tvar_mapping list Constraints.t

let rec substitute (m : tvar_mapping) = function
  | TFun (a, b) -> TFun ((substitute m a), (substitute m b))
  | TTuple l -> TTuple (List.map (substitute m) l)
  | TVar i when (fst m) = i -> snd m
  | t -> t

let substitute_p (m : tvar_mapping) = function
  | Poly (is, t) -> Poly (is, substitute m t)
  | t -> t

let instantiate : tc_type_p -> tc_type = function
  | Poly (is, t) -> List.fold_left (fun acc i -> substitute (i, fresh_var ()) acc) t is
  | Mono t -> t

let unify_constraint_mono : (tc_type * tc_type) -> unify_result = function
  | (TVar v1), b -> (match b with
      | TVar v2 when v1 = v2 -> []
      | _ -> if occurs_in v1 b then raise (TCError "Infinite types are unsupported") else [(v1, b)])
    |> Constraints.return
  | (TFun (arg_a, body_a)), (TFun (arg_b, body_b)) -> [mono_c (arg_a, arg_b); mono_c (body_a, body_b)] |> Constraints.make []
  | (TTuple a), (TTuple b) when List.length a = List.length b -> List.map2 (fun at bt -> mono_c (at, bt)) a b |> Constraints.make []
  | TInt, TInt | TBool, TBool | TString, TString | TUnit, TUnit -> Constraints.return []
  | a, b -> raise (TCError ("Failed to unify types " ^ show_tc_type a ^ " and " ^ show_tc_type b))

let unify_constraint ((a, b) : Constraints.entry) : unify_result =
  unify_constraint_mono (instantiate a, instantiate b)

let unify (l : Constraints.entry list) : unify_result =
  let rec unify_inner ((mappings, constraints) as r : unify_result) = match constraints with
    | c :: t ->
      let (new_mappings, new_constraints) = unify_constraint c in
      Constraints.make (new_mappings @ mappings)
        (List.map (fun (a, b) ->
          let f t = List.fold_left (Fun.flip substitute_p) t new_mappings in (f a, f b)
        ) (new_constraints @ t))
      |> unify_inner
    | [] -> r
  in Constraints.make [] l |> unify_inner

let generalize (env : Env.t) (l : Constraints.entry list) (t : tc_type_p) : tc_type_p =
  match t with
    | Poly _ -> t
    | Mono t ->
  let mappings = unify l |> Constraints.value in
  let env1 = List.map (fun (k, t) -> (k, List.fold_left (Fun.flip substitute_p) t mappings)) env in
  let t1 = List.fold_left (Fun.flip substitute) t mappings in
  let rec collect_vars = function
    | TInt | TBool | TString | TUnit | TId _ -> []
    | TFun (a, b) -> (collect_vars a) @ (collect_vars b)
    | TTuple l -> List.concat_map collect_vars l
    | TVar i -> if (List.map snd env1 |> List.exists (occurs_in_p i)) then [] else [i]
  in
  Poly (collect_vars t1, t1)

let rec get_constraints (env : Env.t) : ol_expr_l2 -> tc_type_p Constraints.t =
  let get_c = get_constraints env in function
  | LetExpr { id; is_rec; t; expr; body } ->
    let (t_val, t_c) = get_constraints env expr in
    let g = generalize env t_c t_val in
    let t_body = get_constraints ((id, g) :: env) body in
    Constraints.join t_c t_body |> Constraints.add_opt t_val (map_mono t)
    (*let t_b = fresh_var () in
    let* t_val = get_constraints (env |> Env.add_opt id (if is_rec then Some t_b else None)) expr in
    let t_body = get_constraints ((id, t_b) :: env) body in t_body |> Constraints.add (t_b, t_val)*)
  | TypeBindingExpr { t = { id; t }; body } ->
    get_constraints ((List.map (fun ({ id = c_id; t } : ol_id_with_t_l2) -> (c_id, Mono (match t with
      | None -> TId id
      | Some t -> TFun (t, TId id)
    ))) t) @ env) body
  | FunExpr { params; t; e } ->
    let (param, body) = (match params with
      | [p] -> (p, e)
      | p :: tail -> (p, FunExpr { params = tail; t; e; })
      | _ -> failwith "Received function with no params"
    ) in
    let t_arg = fresh_var () in
    let body_env = [(param.id, Mono t_arg)] |> Env.add_opt param.id (map_mono param.t) in
    let* t_body = get_constraints body_env body in
    Constraints.return (Mono (TFun (t_arg, unwrap_p t_body))) |> Constraints.add_opt t_body (map_mono t)
  | ApplExpr { f; a } ->
    let t = fresh_var () in
    let* t_f = get_c f in
    let* t_a = get_c a in
    Constraints.make (Mono t) [(t_f, Mono (TFun (unwrap_p t_a, t)))]
  | IfExpr { cond; e_if; e_else } ->
    let* t_cond = get_c cond in
    let* t_if = get_c e_if in
    let* t_else = get_c e_else in
    let v = fresh_var () in
    Constraints.make (Mono v) [mono_c (TBool, unwrap_p t_cond); mono_c (v, unwrap_p t_if); mono_c (v, unwrap_p t_else)]
  | TupleExpr l ->
    let+ v = List.map get_c l |> List.fold_left (fun acc cur ->
      let* a_acc = acc in
      let+ t_cur = cur in
      (unwrap_p t_cur) :: a_acc
    ) (Constraints.return []) in
     Mono (TTuple v)
  (*| MatchExpr { e; branches } ->
    let* t_e = get_c e in
*)
  | v -> Constraints.return (match v with
    | IdExpr i -> env |> Env.get i
    | IntExpr _ -> Mono TInt
    | StringExpr _ -> Mono TString
    | BoolExpr _ -> Mono TBool
    | UnitExpr -> Mono TUnit
  )

(* let is_well_typed e = e |> get_type_of |> Result.is_ok *)