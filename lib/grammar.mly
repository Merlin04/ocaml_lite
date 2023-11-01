%{

open Ast

let build_appl = function
  | h :: t -> List.fold_left (fun acc cur -> ApplExpr { f = acc; a = cur }) h t
  | _ -> failwith "build_appl called on empty list"

%}

%token Type "type"
%token Of "of"
%token Let "let"
%token Rec "rec"
%token In "in"
%token If "if"
%token Then "then"
%token Else "else"
%token Match "match"
%token With "with"
%token Fun "fun"
%token True "true"
%token False "false"
%token Mod "mod"
%token TInt "int"
%token TBool "bool"
%token TString "string"
%token TUnit "unit"
%token Eq "=="
%token Plus "+"
%token Minus "-"
%token Times "*"
%token Divide "/"
%token Lt "<"
%token Concat "^"
%token And "&&"
%token Or "||"
%token Not "not"
%token Negate "~"
%token DoubleSemicolon ";;"
%token Colon ":"
%token Arrow "->"
%token DoubleArrow "=>"
%token LParen "("
%token RParen ")"
%token Pipe "|"
%token Comma ","
%token <string> Id "id"
%token <int> Int "5"
%token <string> String "\"hello\""
%token EOF

%start <ol_prog> prog

%type <ol_let> let_component
%type <ol_id_with_t> type_binding_constructor
%type <ol_binding> binding
%type <ol_id_with_t> param
%type <ol_expr> expr, expr_appl, expr_other
%type <ol_match_branch> match_branch
%type <string list> pattern_vars
%type <ol_type> ol_type
%type <ol_binop> binop
%type <ol_unop> unop

%nonassoc In
%nonassoc Else
%right Arrow
%right DoubleArrow

%left Or
%left And
%left Lt, Eq
%left Plus, Minus, Concat
%left Times, Divide, Mod
%nonassoc Negate
%nonassoc Not

%%

(* List parser functions that were taken from the OCaml parser *)
(* see https://github.com/ocaml/ocaml/blob/trunk/parsing/parser.mly#L1122 *)
let reversed_separated_nontrivial_llist(separator, X) :=
  | xs = reversed_separated_nontrivial_llist(separator, X); separator; x = X; { x :: xs }
  | x1 = X; separator; x2 = X; { [x2; x1] }

let reversed_nonempty_llist(X) :=
  | x = X; { [ x ] }
  | xs = reversed_nonempty_llist(X); x = X; { x :: xs }

let nonempty_llist(X) :=
  | ~ = rev(reversed_nonempty_llist(X)); <>


let prog :=
  | EOF; { [] }
  | b = binding; DoubleSemicolon; p = prog; { b :: p }

let let_component :=
  | Let; r = Rec?; id = Id; params = param*; t = option(Colon; ol_type); Eq; ~ = expr; { { id; is_rec = r <> None; params; t; expr } }

let type_binding_constructor :=
  | id = Id; t = option(Of; ol_type); { { id; t } }

let binding :=
  | l = let_component; { LetBinding (l) }
  | Type; id = Id; Eq; Pipe?; t = separated_nonempty_list(Pipe, type_binding_constructor); { TypeBinding ({ id; t }) }

let param :=
  | id = Id; { { id; t = None } }
  | LParen; id = Id; Colon; t = ol_type; RParen; { { id; t = Some t } }

let expr :=
  | ~ = expr_appl; <>
  | ~ = expr_floating; <>
  | ~ = expr_other; <>

let expr_appl :=
  | f = expr_other; a = nonempty_llist(expr_other); { (f :: a) |> build_appl }

(* "floating" expressions which can't be inside an application because it has an expr on the outside of it *)
let expr_floating :=
  | a = expr; op = binop; b = expr; { BinopExpr { a; op; b } }
  | op = unop; e = expr; { UnopExpr { op; e } }
  | l = let_component; In; e = expr; { LetExpr ({ l; e }) }
  | Fun; params = param+; t = option(Colon; ol_type); DoubleArrow; e = expr; { FunExpr { params; t; e } }
  | If; cond = expr; Then; e_if = expr; Else; e_else = expr; { IfExpr { cond; e_if; e_else } }
  | Match; e = expr; With; Pipe?; b = separated_nonempty_list(Pipe, match_branch); { MatchExpr { e; branches = b } }

let match_branch :=
  | id = Id; vars = pattern_vars?; DoubleArrow; e = expr; { { id; vars = Option.value ~default:[] vars; e } }

let pattern_vars :=
  | id = Id; { [id] }
  | LParen; ~ = separated_nonempty_list(Comma, Id); RParen; <>

let expr_other :=
  | LParen; h = expr; Comma; t = separated_nonempty_list(Comma, expr); RParen; { TupleExpr (h :: t) }
  | LParen; ~ = expr; RParen; <>
  | ~ = Int; <IntExpr>
  | True; { BoolExpr true }
  | False; { BoolExpr false }
  | ~ = String; <StringExpr>
  | ~ = Id; <IdExpr>
  | LParen; RParen; { UnitExpr }

let ol_type :=
  | ~ = ol_type_fun; <>
  | ~ = ol_type_tuple; <>
  | ~ = ol_type_other; <>

let ol_type_other :=
  | LParen; ~ = ol_type; RParen; <>
  | TInt; { IntType }
  | TBool; { BoolType }
  | TString; { StringType }
  | TUnit; { UnitType }
  | ~ = Id; <IdType>

let ol_type_fun :=
  | a = ol_type; Arrow; b = ol_type; <FunType>

let ol_type_tuple :=
  ~ = rev(reversed_separated_nontrivial_llist(Times, ol_type_other)); <TupleType>

let binop ==
  | Plus; { Plus }
  | Minus; { Minus }
  | Times; { Times }
  | Divide; { Divide }
  | Mod; { Mod }
  | Lt; { Lt }
  | Eq; { Eq }
  | Concat; { Concat }
  | And; { And }
  | Or; { Or }

let unop ==
  | Not; { Not }
  | Negate; { Negate }