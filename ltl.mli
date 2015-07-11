type ltl =
  | LTLposatom of string
  | LTLnegatom of string
  | LTLconj of ltl list
  | LTLdisj of ltl list
  | LTLnext of ltl
  | LTLuntil of ltl * ltl
  | LTLrelease of ltl * ltl

(* val ltl_cache : (ltl, ltl) Hashtbl.t *)

(* val ltl_intern : ltl -> ltl *)

val ltl_atom : string -> ltl
val ltl_top : ltl
val ltl_bot : ltl
val ltl_conj2 : ltl -> ltl -> ltl
val ltl_disj2 : ltl -> ltl -> ltl
val ltl_next : ltl -> ltl
val ltl_future : ltl -> ltl
val ltl_global : ltl -> ltl
val ltl_until : ltl -> ltl -> ltl
val ltl_release : ltl -> ltl -> ltl
val ltl_neg : ltl -> ltl
val ltl_impl : ltl -> ltl -> ltl
val ltl_equiv : ltl -> ltl -> ltl

val pp_ltl : Format.formatter -> ltl -> unit
