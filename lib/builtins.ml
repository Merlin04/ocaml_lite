open Ast
open Tc_type

exception RuntimeError of string

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

let builtin p f =
  ClosureVal { params = p; expr = BuiltinFunExpr f |> IContext.return; rec_symbol = None }

let builtin_unop f = builtin ["a"] (fun c -> let a = List.assoc "a" c in f a)
let builtin_binop f = builtin ["a"; "b"] (fun c -> let a = List.assoc "a" c in let b = List.assoc "b" c in f a b)

let make_builtins l = (List.map (fun (a, b, c) -> (a, b)) l, List.map (fun (a, b, c) -> (a, c)) l)

type context_entry_list = IContext.entry list
[@@deriving show]

let (builtin_env, builtin_icontext) = make_builtins [
  ("int_of_string", TFun (TString, TInt), builtin_unop (fun v -> let s = unwrap_string v in IntVal (int_of_string s)));
  ("string_of_int", TFun (TInt, TString), builtin_unop (fun v -> let i = unwrap_int v in StringVal (string_of_int i)));
  ("print_string", TFun (TString, TUnit), builtin_unop (fun v -> let s = unwrap_string v in print_endline s; UnitVal));
  ("print_debug_val", TForall ("'0", TFun (TVar "'0", TUnit)), builtin_unop (fun v -> v |> show_ol_val |> print_endline; UnitVal));
  ("print_context", TFun (TUnit, TUnit), builtin ["u"] (fun c -> show_context_entry_list c |> print_endline; UnitVal));
  (id_of_binop Plus, TFun (TInt, TFun (TInt, TInt)), builtin_binop (fun a b -> IntVal (unwrap_int a + unwrap_int b)));
  (id_of_binop Minus, TFun (TInt, TFun (TInt, TInt)), builtin_binop (fun a b -> IntVal (unwrap_int a - unwrap_int b)));
  (id_of_binop Times, TFun (TInt, TFun (TInt, TInt)), builtin_binop (fun a b -> IntVal (unwrap_int a * unwrap_int b)));
  (id_of_binop Divide, TFun (TInt, TFun (TInt, TInt)), builtin_binop (fun a b -> IntVal (unwrap_int a / unwrap_int b)));
  (id_of_binop Mod, TFun (TInt, TFun (TInt, TInt)), builtin_binop (fun a b -> IntVal (unwrap_int a mod unwrap_int b)));
  (id_of_binop Lt, TFun (TInt, TFun (TInt, TBool)), builtin_binop (fun a b -> BoolVal (unwrap_int a < unwrap_int b)));
  (id_of_binop Eq, TForall ("'0", TFun (TVar "'0", TFun (TVar "'0", TBool))), builtin_binop (fun a b -> BoolVal (a = b)));
  (id_of_binop Concat, TFun (TString, TFun (TString, TString)), builtin_binop (fun a b -> StringVal (unwrap_string a ^ unwrap_string b)));
  (id_of_binop And, TFun (TBool, TFun (TBool, TBool)), builtin_binop (fun a b -> BoolVal (unwrap_bool a && unwrap_bool b)));
  (id_of_binop Or, TFun (TBool, TFun (TBool, TBool)), builtin_binop (fun a b -> BoolVal (unwrap_bool a || unwrap_bool b)));
  (id_of_unop Not, TFun (TBool, TBool), builtin_unop (fun a -> BoolVal (not (unwrap_bool a))));
  (id_of_unop Negate, TFun (TInt, TInt), builtin_unop (fun a -> IntVal (- (unwrap_int a))));
]