%{

type ol_type =
  | FunType of ol_type * ol_type
  | TupleType of ol_type * ol_type
  | IntType
  | BoolType
  | StringType
  | UnitType
  | IdType of string
[@@deriving show]
type ol_binop = Plus | Minus | Times | Divide | Mod | Lt | Eq | Concat | And | Or
[@@deriving show]
type ol_unop = Not | Negate
[@@deriving show]
type ol_id_with_t = { id : string; t : ol_type option }
[@@deriving show]
type ol_let = {
  id : string;
  is_rec : bool;
  params : ol_id_with_t list;
  t : ol_type option;
  expr : ol_expr }
[@@deriving show]
and ol_binding =
  | TypeBinding of {
    id : string;
    t : ol_id_with_t list }
  | LetBinding of ol_let
[@@deriving show]
and ol_match_branch = {
 id : string;
 vars : string list;
 e : ol_expr }
[@@deriving show]
and ol_expr =
  | LetExpr of { l : ol_let; e : ol_expr }
  | IfExpr of { cond : ol_expr; e_if : ol_expr; e_else : ol_expr }
  | FunExpr of { params : ol_id_with_t list; t : ol_type option; e : ol_expr }
  | ApplExpr of { f : ol_expr; a : ol_expr }
  | TupleExpr of ol_expr list
  | BinopExpr of { a : ol_expr; op : ol_binop; b : ol_expr }
  | UnopExpr of { op : ol_unop; e : ol_expr }
  | MatchExpr of { e : ol_expr; branches : ol_match_branch list }
  | IntExpr of int
  | BoolExpr of bool
  | StringExpr of string
  | UnitExpr
  | IdExpr of string
[@@deriving show]

type ol_prog = ol_binding list
[@@deriving show]
%}

%token Type
%token Of
%token Let
%token Rec
%token In
%token If
%token Then
%token Else
%token Match
%token With
%token Fun
%token True
%token False
%token Mod
%token TInt
%token TBool
%token TString
%token TUnit
%token Eq
%token Plus
%token Minus
%token Times
%token Divide
%token Lt
%token Concat
%token And
%token Or
%token Not
%token Negate
%token DoubleSemicolon
%token Colon
%token Arrow
%token DoubleArrow
%token LParen
%token RParen
%token Pipe
%token Comma
%token <string> Id
%token <int> Int
%token <string> String
%token EOF

%start <ol_prog> prog

%type <ol_let> let_component
%type <ol_id_with_t> type_binding_constructor
%type <ol_binding> binding
%type <ol_id_with_t> param
%type <ol_expr> expr, expr_app, expr_other, match_expr
%type <ol_match_branch> match_branch
%type <string list> pattern_vars
%type <ol_type> ol_type
%type <ol_binop> binop
%type <ol_unop> unop

%left Application
// %right DoubleArrow // the precedence rule being here fixes the below issue, but causes associativity of matches to break
%left Or
%left And
%left Lt, Eq
%left Plus, Minus, Concat
%left Times, Divide, Mod
%left Negate
%left Not
%right Arrow
%right Else
%right DoubleArrow // the precedence rule being here causes | a => a + a to result in "unexpected token" on the next |
%right In

%%

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
  | ~ = expr_app; <>

let expr_app :=
  | f = expr; a = expr_other; { ApplExpr { f; a } } %prec Application
  | ~ = expr_other; <>

let expr_other :=
  | l = let_component; In; e = expr; { LetExpr ({ l; e }) }
  | If; cond = expr; Then; e_if = expr; Else; e_else = expr; { IfExpr { cond; e_if; e_else } }
  | Fun; params = param+; t = option(Colon; ol_type); DoubleArrow; e = expr; { FunExpr { params; t; e } }
  | LParen; h = expr; Comma; t = separated_nonempty_list(Comma, expr); RParen; { TupleExpr (h :: t) }
  | a = expr; op = binop; b = expr; { BinopExpr { a; op; b } }
  | op = unop; e = expr; { UnopExpr { op; e } }
  | LParen; ~ = expr; RParen; <>
  | ~ = Int; <IntExpr>
  | True; { BoolExpr true }
  | False; { BoolExpr false }
  | ~ = String; <StringExpr>
  | ~ = Id; <IdExpr>
  | LParen; RParen; { UnitExpr }
  | ~ = match_expr; <>

let match_expr :=
  | Match; e = expr; With; Pipe?; b = separated_nonempty_list(Pipe, match_branch); { MatchExpr { e; branches = b } }

let match_branch :=
  | id = Id; vars = pattern_vars?; DoubleArrow; e = expr; { { id; vars = Option.value ~default:[] vars; e } }

let pattern_vars :=
  | id = Id; { [id] }
  | LParen; ~ = separated_nonempty_list(Comma, Id); RParen; <>

let ol_type :=
  | a = ol_type; Arrow; b = ol_type; <FunType>
  | LParen; ~ = ol_type; RParen; <>
  | a = ol_type; Times; b = ol_type; <TupleType>
  | TInt; { IntType }
  | TBool; { BoolType }
  | TString; { StringType }
  | TUnit; { UnitType }
  | ~ = Id; <IdType>

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