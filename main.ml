open Ltl

let () =
  let lexbuf = Lexing.from_channel stdin in
  let tn = Ltl_parser.main Ltl_lexer.token lexbuf in
  ()
