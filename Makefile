#!/usr/bin/make -f

SOURCES := \
	ltl.mli ltl.ml \
	ltl_parser.mly \
	ltl_lexer.mll \
	positive_formula.mli positive_formula.ml \
	alt_buechi.mli alt_buechi.ml \
	buechi.mli buechi.ml \
	main.ml

OCAMLYACC := menhir

RESULT := ltl

OCAMLFLAGS := -w Aelyz

include OCamlMakefile

