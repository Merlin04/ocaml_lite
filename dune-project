(lang dune 3.8)

(name ocaml_lite)

(generate_opam_files true)

(authors "B Smith")

(license BSD3)

(package
 (name ocaml_lite)
 (synopsis "OCaml-lite language.")
 (depends ocaml dune menhir ppx_deriving nice_parser))

(using menhir 2.1)