type positive_formula =
  | PFatom of int
  | PFtop
  | PFconj
  | PFbot
  | PFdisj
type alt_buechi = {
  alphabet_size : int;
  num_states : int;
  start : int;
  next : (int * int, positive_formula) Hashtbl.t;
  accepts : (int, bool) Hashtbl.t
}
