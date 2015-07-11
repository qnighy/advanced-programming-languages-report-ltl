open Ltl
open Positive_formula
open Alt_buechi
open Buechi

let () =
  let lexbuf = Lexing.from_channel stdin in
  let tn = Ltl_parser.main Ltl_lexer.token lexbuf in
  Format.printf "ltl@ =@ %a@." pp_ltl tn;
  let (am, atomhash, rev_atomhash) = alt_buechi_from_ltl tn in
  Format.printf "alt_buechi@ =@ %a@." (pp_alt_buechi atomhash rev_atomhash) am;
  let m = buechi_from_alt am in
  Format.printf "buechi@ =@ %a@." (pp_buechi atomhash rev_atomhash) m
