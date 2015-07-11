open Ltl
open Positive_formula

type alt_buechi = {
  alphabet_size : int;
  mutable num_states : int;
  mutable start : int;
  next : (int * int, positive_formula) Hashtbl.t;
  accepts : (int, bool) Hashtbl.t
}

(*
val ltl_construct_atomhash : (string, int) Hashtbl.t -> ltl -> unit
val alt_buechi_from_ltl_createstate :
  alt_buechi -> (ltl, int) Hashtbl.t -> (string, int) Hashtbl.t ->
  ltl -> int
val alt_buechi_from_ltl_createatom :
  alt_buechi -> (ltl, int) Hashtbl.t -> (string, int) Hashtbl.t ->
  ltl -> positive_formula
val alt_buechi_from_ltl_createnext :
  alt_buechi -> (ltl, int) Hashtbl.t -> (string, int) Hashtbl.t ->
  int -> ltl -> positive_formula
*)

val alt_buechi_from_ltl :
  ltl -> alt_buechi * (string, int) Hashtbl.t * (int, string) Hashtbl.t

val pp_alt_buechi :
  (string, int) Hashtbl.t -> (int, string) Hashtbl.t ->
    Format.formatter -> alt_buechi -> unit
