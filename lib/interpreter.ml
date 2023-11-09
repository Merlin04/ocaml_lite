open Ast

exception RuntimeError of string

(* really awful and janky way to get ol_val into the top level scope *)
(* (moving Context module into its own file and using ppx_import to copy ol_val in using this weird ocaml feature *)
(* https://github.com/ocaml-ppx/ppx_deriving#working-with-existing-types *)
(* I couldn't figure out how to just define it separately due to the mutual dependency between the type and the module *)
(* if you have any idea how to do this please tell me! *)
type ol_val = [%import: Context.ol_val [@with t := Context.t]]

let expr_of_ol_let (o : ol_let) = match o.params with
  | [] -> o.expr
  | _ -> FunExpr { params = o.params; t = None; e = o.expr }

let unwrap_int = function
  | IntVal v -> v
  | _ -> raise (RuntimeError "Expected int")
let unwrap_string = function
  | StringVal v -> v
  | _ -> raise (RuntimeError "Expected string")
let unwrap_bool = function
  | BoolVal v -> v
  | _ -> raise (RuntimeError "Expected bool")
let unwrap_unit = function
  | UnitVal -> UnitVal
  | _ -> raise (RuntimeError "Expected unit")

(*let rec val_eq a b = match a with
  | IntVal a_val -> (match b with IntVal b_val -> a_val = b_val | _ -> false)
  | StringVal a_val -> (match b with StringVal b_val -> a_val = b_val | _ -> false)
  | BoolVal a_val -> (match b with BoolVal b_val -> a_val = b_val | _ -> false)
  | UnitVal -> (match b with UnitVal -> true | _ -> false)
  | TupleVal a_val -> (match b with
    | TupleVal b_val -> (try List.for_all2 val_eq a_val b_val with Invalid_argument -> false)
    | _ -> false)
  | VariantVal a_id, a_val -> (match b with VariantVal b_id, b_val -> a_id = b_id && val_eq a_val b_val | _ -> false)
  | ConstructorVal a_id -> (match b with ConstructorVal b_id -> a_id = b_id | _ -> false)
  |*)

let bool_binop_fn = function
  | And -> fun a b -> BoolVal (a && b)
  | Or -> fun a b -> BoolVal (a || b)
  | _ -> failwith "binop does not take bools"

let int_binop_fn = function
  | Plus -> fun a b -> IntVal (a + b)
  | Minus -> fun a b -> IntVal (a - b)
  | Times -> fun a b -> IntVal (a * b)
  | Divide -> fun a b -> IntVal (a / b)
  | Mod -> fun a b -> IntVal (a mod b)
  | Lt -> fun a b -> BoolVal (a < b)
  | _ -> failwith "binop does not take ints"

let id_is_constructor id = match String.get id 0 with
  | 'A'..'Z' -> true
  | _ -> false

let rec interpret_expr (in_e : ol_expr Context.t) : ol_val =
  let with_e_ctx v = Context.from v in_e in
  let interpret v = v |> with_e_ctx |> interpret_expr in
  match Context.value in_e with
    | LetExpr { l; e } ->
      let bound_val =
        let i = expr_of_ol_let l |> interpret in
        if l.is_rec then match i with
          | ClosureVal v -> ClosureVal { v with rec_symbol = Some l.id }
          | _ -> raise (RuntimeError "Only expressions which evaluate to closures can be recursive")
        else i in
      e |> with_e_ctx |> Context.add (l.id, bound_val) |> interpret_expr
    | FunExpr { params; e; _ } ->
      ClosureVal { params = List.map (fun (p : ol_id_with_t) -> p.id) params; expr = with_e_ctx e; rec_symbol = None }
    | ApplExpr { f; a } ->
      let a_val = interpret a in
      let fun_val = interpret f in
      (match fun_val with
        | ClosureVal { params; expr; rec_symbol } ->
          (match params with
            | h :: t -> let closure_expr_c = expr |> Context.add (h, a_val) in (match t with
              | [] -> (match rec_symbol with Some self -> closure_expr_c |> Context.add (self, fun_val) | None -> closure_expr_c) |> interpret_expr
              | _ -> ClosureVal { rec_symbol; params = t; expr = closure_expr_c })
            | [] -> raise (RuntimeError "function value has no parameters"))
        | ConstructorVal c -> VariantVal (c, a_val)
        | _ -> raise (RuntimeError "Attempted to apply argument to an expression which is not a closure or constructor"))
    | IfExpr { cond; e_if; e_else } ->
      (match interpret cond with
        | BoolVal b -> (if b then e_if else e_else) |> interpret
        | _ -> raise (RuntimeError "If-expression condition must be a boolean"))
    | TupleExpr l -> TupleVal (List.map interpret l)
    | BinopExpr { a; op; b } ->
      let a_val = interpret a in let b_val = interpret b in
      (match op with
        | Concat -> let a_str = unwrap_string a_val in let b_str = unwrap_string b_val in StringVal (a_str ^ b_str)
        | Eq -> BoolVal(a_val = b_val)
        | And | Or -> let a_bool = unwrap_bool a_val in let b_bool = unwrap_bool b_val in bool_binop_fn op a_bool b_bool
        | _ -> let a_int = unwrap_int a_val in let b_int = unwrap_int b_val in int_binop_fn op a_int b_int)
    | UnopExpr { op; e } ->
      let e_val = interpret e in
      (match op with
        | Negate -> let e_int = unwrap_int e_val in IntVal (-e_int)
        | Not -> let e_bool = unwrap_bool e_val in BoolVal (not e_bool))
    | MatchExpr { e; branches } ->
      let e_val = interpret e in
      let binding_to_entry id v =
        if id = "_" then None else Some (id, v) in
      let check_branch b =
        let branch_c = with_e_ctx b.e in
        if not (id_is_constructor b.id) then
          (match binding_to_entry b.id e_val with Some e -> Context.add e branch_c | None -> branch_c)
            |> interpret_expr |> Option.some
        else match e_val with
          | ConstructorVal s when s = b.id -> Some (interpret b.e)
          | VariantVal (s, v_val) when s = b.id -> let branch_c = with_e_ctx b.e in (match b.vars with
            | [b_var] -> branch_c |> Context.add (b_var, v_val) |> interpret_expr |> Option.some
            | [] -> raise (RuntimeError "Attempting to match a non-constant constructor against a pattern with no constructor arguments - maybe you meant to add an _?")
            | _ -> match v_val with
              | TupleVal v_vals ->
                let entries = try List.map2 binding_to_entry b.vars v_vals |> List.filter_map Fun.id (* because there's no List.filter_map2 *)
                  with Invalid_argument _ -> raise (RuntimeError "Attempting to match a tuple against tuple pattern with different length") in
                branch_c |> Context.join entries |> interpret_expr |> Option.some
              | _ -> raise (RuntimeError "Can't match a non-tuple value against a tuple pattern"))
          | _ -> None in
      (match List.find_map check_branch branches with
        | Some v -> v
        | None -> raise (RuntimeError "No case in match expression matched input"))
    | IntExpr i -> IntVal i
    | BoolExpr b -> BoolVal b
    | StringExpr s -> StringVal s
    | UnitExpr -> UnitVal
    | IdExpr s ->
      if id_is_constructor s
        then ConstructorVal s
        else match Context.get s in_e with
          | Some v -> v
          | None -> raise (RuntimeError ("Unbound symbol " ^ s))

let bindings_to_expr (p : ol_prog) =
  let rec help = function
    | l :: t -> LetExpr { l; e = match t with [] -> IdExpr l.id | _ -> help t }
    | _ -> (* empty program *) UnitExpr in
  List.filter_map (function LetBinding l -> Some l | _ -> None) p |> help

let interpret_prog p = p |> bindings_to_expr |> Context.return |> interpret_expr