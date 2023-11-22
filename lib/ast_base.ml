type 't ol_id_with_t_base = { id : string; t : 't option }
[@@deriving show]
type 't ol_type_binding_base = { id : string; t : 't ol_id_with_t_base list }
[@@deriving show]
type 'e ol_match_branch_base = {
  id : string;
  vars : string list;
  e : 'e }
[@@deriving show]
type ('e, 't) ol_fun_base = { params : 't ol_id_with_t_base list; t : 't option; e : 'e }
[@@deriving show]
type 'e ol_appl_base = { f : 'e; a : 'e }
[@@deriving show]
type 'e ol_if_base = { cond : 'e; e_if : 'e; e_else : 'e }
[@@deriving show]
type 'e ol_match_base = { e : 'e; branches : 'e ol_match_branch_base list }
[@@deriving show]