open Ltl
open Positive_formula
open Alt_buechi

type buechi = {
  alphabet_size : int;
  mutable num_states : int;
  mutable start : int list;
  next : (int * int, int list) Hashtbl.t;
  accepts : (int, bool) Hashtbl.t
}

(*
val buechi_from_alt_createstate :
  alt_buechi -> buechi -> (int * int, int) Hashtbl.t -> int -> int -> int ->
    int
*)

val buechi_from_alt : alt_buechi -> buechi

val pp_buechi :
  (string, int) Hashtbl.t -> (int, string) Hashtbl.t ->
    Format.formatter -> buechi -> unit

val pp_buechi_graphviz :
  (string, int) Hashtbl.t -> (int, string) Hashtbl.t ->
    Format.formatter -> buechi -> unit
