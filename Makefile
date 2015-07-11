#!/usr/bin/make -f

SOURCES := \
	ltl.ml \
	ltl_parser.mly \
	ltl_lexer.mll \
	automata.ml \
	main.ml

OCAMLYACC := menhir

RESULT := ltl

OCAMLFLAGS := -w Aelyz

include OCamlMakefile

