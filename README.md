# f90

original repo https://github.com/nomaddo/f90
a tiny subset of fortran 90 written in ocaml.

This project aim to support all feature in fortran 90.

## build
Requrement
- ocamlfind
- menhir
- ppx_sexp_conv

`opam install ppx_sexp_conv ocamlfind menhir ppx_deriving sexplib`

## running

`-C` flag is used indicate a directory: [docs](https://www.gnu.org/software/make/manual/html_node/Options-Summary.html
)
`make -C ../ test` 