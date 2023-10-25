(*
To get ready for the project, let's write a little bit of OCaml-lite code and think about how it would be represented as an AST.

Remember the suffix_sum exercise from our OCaml practice: we wanted to take a list of integers and produce a list which contained the sums of suffixes of the list. For example

suffix_sum [1; 2; 3; 4; 5] = [15; 14; 12; 9; 5]

Write this function in OCaml-lite. This will involve defining a list data type for integers (you can look at the example code below).

Now think about how your code could be represented as an AST, and specifically how you might write it in OCaml using a set of algebraic data types.

Write code and think about it's representation for the propositions exercise from the OCaml homework. Remember that in this exercise, your goal was to develop a data type that represented propositions and then to write a function pprint to pretty-print those propositions.
*)

type int_list = | Nil | Cons of int * int_list ;;

let rec fold_right (fn : int -> int_list -> int_list) lst i = match lst with
  | Nil -> i
  | Cons (h, t) -> fold_right fn t (fn h i) ;;

let suffix_sum (lst : int_list) =
  fold_right (fun cur acc -> Cons ((cur + (match acc with
    | Nil -> 0
    | Cons (e, _) -> e)), acc)
  ) lst Nil ;;