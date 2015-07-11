type positive_formula =
  | PFatom of int
  | PFconj of positive_formula list
  | PFdisj of positive_formula list

val pf_top : positive_formula
val pf_bot : positive_formula
val pf_conj2 : positive_formula -> positive_formula -> positive_formula
val pf_disj2 : positive_formula -> positive_formula -> positive_formula
val pf_conj : positive_formula list -> positive_formula
val pf_disj : positive_formula list -> positive_formula

val pp_positive_formula : Format.formatter -> positive_formula -> unit

type alt_buechi = {
  alphabet_size : int;
  mutable num_states : int;
  mutable start : int;
  next : (int * int, positive_formula) Hashtbl.t;
  accepts : (int, bool) Hashtbl.t
}

(*
val ltl_construct_atomhash : (string, int) Hashtbl.t -> Ltl.ltl -> unit
val alt_buechi_from_ltl_createstate :
  alt_buechi -> (Ltl.ltl, int) Hashtbl.t -> (string, int) Hashtbl.t ->
  Ltl.ltl -> int
val alt_buechi_from_ltl_createatom :
  alt_buechi -> (Ltl.ltl, int) Hashtbl.t -> (string, int) Hashtbl.t ->
  Ltl.ltl -> positive_formula
val alt_buechi_from_ltl_createnext :
  alt_buechi -> (Ltl.ltl, int) Hashtbl.t -> (string, int) Hashtbl.t ->
  int -> Ltl.ltl -> positive_formula
*)

val alt_buechi_from_ltl :
  Ltl.ltl -> alt_buechi * (string, int) Hashtbl.t * (int, string) Hashtbl.t

val pp_alt_buechi :
  (string, int) Hashtbl.t -> (int, string) Hashtbl.t ->
    Format.formatter -> alt_buechi -> unit
