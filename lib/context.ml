(* context module for interpreter *)

open Ast

type ol_val =
  | IntVal of int
  | StringVal of string
  | BoolVal of bool
  | UnitVal
  | TupleVal of ol_val list
  | VariantVal of string * ol_val (* non-constant constructor applied to an argument *)
  | ConstructorVal of string (* constant constructor, or non-constant constructor that hasn't been applied yet - treated like a function *)
  | ClosureVal of { params : string list; expr : ol_expr t; rec_symbol : string option; } (*[@printer fun fmt _ -> Format.pp_print_string fmt funval_str]*)
[@@deriving show { with_path = false }]
and entry = (string * ol_val)
and 'a t = 'a * (entry list)

let return s = (s, [])
let make s c : 'a t = (s, c)
let value (a : 'a t) = fst a
let context (a : 'a t) = snd a
let from s (c : 'a t) = make s (context c)
(* bind *)
(* 	let ( >>= ) (a : 'a t) (f : 'a -> 'b t) : 'b t = let v, l2 = a |> fst |> f in (v, l2 @ (snd a)) *)
(* 	let ( let* ) = ( >>= ) *)
(* map *)
(* 	let ( >|= ) (a : 'a t) (f : 'a -> 'b) : 'b t = let v, l = a in (f v, l) *)
(* 	let ( let+ ) = ( >|= ) *)
(* map for monad contents *)
let map2 (a : 'a t) (f : entry list -> entry list) = let v, l = a in (v, f l)
let ( let- ) = ( map2 )
let add (e : entry) (a : 'a t) = let- c = a in e :: c
let join (e : entry list) (a : 'a t) = let- c = a in e @ c
let get (k : string) (a : 'a t) = let c = context a in List.assoc_opt k c