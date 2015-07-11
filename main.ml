open Ltl
open Alt_buechi

let () =
  let lexbuf = Lexing.from_channel stdin in
  let tn = Ltl_parser.main Ltl_lexer.token lexbuf in
  Format.printf "ltl@ =@ %a@." pp_ltl tn;
  let (m, atomhash, rev_atomhash) = alt_buechi_from_ltl tn in
  Format.printf "alt_buechi@ =@ %a@." (pp_alt_buechi atomhash rev_atomhash) m
