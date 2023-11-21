open Ast_l2
open Builtins

let id_is_constructor id = match String.get id 0 with
  | 'A'..'Z' -> true
  | _ -> false

let rec interpret_expr (in_e : ol_expr_l2 IContext.t) : ol_val =
  let with_e_ctx v = IContext.from v in_e in
  let interpret v = v |> with_e_ctx |> interpret_expr in
  match IContext.value in_e with
    | LetExpr { id; is_rec; expr; body; _ } ->
      let bound_val =
        let i = interpret expr in
        if is_rec then match i with
          | ClosureVal v -> ClosureVal { v with rec_symbol = Some id }
          | _ -> raise (RuntimeError "Only expressions which evaluate to closures can be recursive")
        else i in
      body |> with_e_ctx |> IContext.add (id, bound_val) |> interpret_expr

    | FunExpr { params; e; _ } ->
      ClosureVal { params = List.map (fun (p : ol_id_with_t_l2) -> p.id) params; expr = with_e_ctx e; rec_symbol = None }

    | ApplExpr { f; a } ->
      let a_val = interpret a in
      let fun_val = interpret f in
      (match fun_val with
        | ClosureVal { params; expr; rec_symbol } ->
          (match params with
            | h :: t -> let closure_expr_c = expr
                |> (match rec_symbol with Some self -> IContext.add (self, fun_val) | None -> Fun.id)
                |> IContext.add (h, a_val) (* argument *)
              in (match t with
                | _ :: _ -> ClosureVal { rec_symbol = None; params = t; expr = closure_expr_c }
                | [] ->
                  closure_expr_c
                  (* add existing context to expression context if we're dealing with a builtin, so we can have things like print_context  *)
                  |> (match IContext.value expr with BuiltinFunExpr _ -> IContext.join_end (IContext.list in_e) | _ -> Fun.id)
                  |> interpret_expr)
            | [] -> raise (RuntimeError "function value has no parameters"))
        | ConstructorVal c -> VariantVal (c, a_val)
        | _ -> raise (RuntimeError "Attempted to apply argument to an expression which is not a closure or constructor"))

    | IfExpr { cond; e_if; e_else } ->
      (match interpret cond with
        | BoolVal b -> (if b then e_if else e_else) |> interpret
        | _ -> raise (RuntimeError "If-expression condition must be a boolean"))

    | TupleExpr l -> TupleVal (List.map interpret l)

    | MatchExpr { e; branches } ->
      let e_val = interpret e in
      let binding_to_entry id v =
        if id = "_" then None else Some (id, v) in
      let check_branch (b : ol_expr_l2 Ast_base.ol_match_branch_base) =
        let branch_c = with_e_ctx b.e in
        if not (id_is_constructor b.id) then
          (match binding_to_entry b.id e_val with Some e -> IContext.add e branch_c | None -> branch_c)
            |> interpret_expr |> Option.some
        else match e_val with
          | ConstructorVal s when s = b.id -> Some (interpret b.e)
          | VariantVal (s, v_val) when s = b.id -> let branch_c = with_e_ctx b.e in (match b.vars with
            | [b_var] -> branch_c |> IContext.add (b_var, v_val) |> interpret_expr |> Option.some
            | [] -> raise (RuntimeError "Attempting to match a non-constant constructor against a pattern with no constructor arguments - maybe you meant to add an _?")
            | _ -> match v_val with
              | TupleVal v_vals ->
                let entries = try List.map2 binding_to_entry b.vars v_vals |> List.filter_map Fun.id (* because there's no List.filter_map2 *)
                  with Invalid_argument _ -> raise (RuntimeError "Attempting to match a tuple against tuple pattern with different length") in
                branch_c |> IContext.join entries |> interpret_expr |> Option.some
              | _ -> raise (RuntimeError "Can't match a non-tuple value against a tuple pattern"))
          | _ -> None in
      (match List.find_map check_branch branches with
        | Some v -> v
        | None -> raise (RuntimeError ("No case in match expression matched input; matching on expression " ^ show_ol_expr_l2 e)))

    | BuiltinFunExpr f -> IContext.list in_e |> f
    | IntExpr i -> IntVal i
    | BoolExpr b -> BoolVal b
    | StringExpr s -> StringVal s
    | UnitExpr -> UnitVal
    | IdExpr s ->
      if id_is_constructor s
        then ConstructorVal s
        else (match IContext.get s in_e with
          | Some v -> v
          | None -> raise (RuntimeError ("Unbound symbol " ^ s)))
(*    | _ -> failwith "Expression should have been transformed away" *)

let interpret_prog_expr p = p
  |> (Fun.flip IContext.make) builtin_icontext
  |> interpret_expr