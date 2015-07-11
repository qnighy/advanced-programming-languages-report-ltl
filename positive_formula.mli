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

val positive_dnf : positive_formula -> int list list
