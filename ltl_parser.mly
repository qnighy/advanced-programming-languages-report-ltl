%{
  open Ltl
%}
%token <string> IDENT
%token STRAY EOF
%token LPAREN RPAREN
%token TOP BOT AND OR IMPLIES EQUIV NOT
%token UNTIL RELEASE FUTURE GLOBAL NEXT
%start main
%type <Ltl.ltl> main
%type <Ltl.ltl> expression

%%

main:
  | e = expression; EOF { e }
expression:
  | e = expression10 { e }
expression10:
  | e1 = expression9; EQUIV; e2 = expression9 { ltl_equiv e1 e2 }
  | e1 = expression9; IMPLIES; e2 = expression10 { ltl_impl e1 e2 }
  | e = expression9 { e }
expression9:
  | e = expression8 { e }
expression8:
  | e1 = expression7; UNTIL; e2 = expression7 { ltl_until e1 e2 }
  | e1 = expression7; RELEASE; e2 = expression7 { ltl_release e1 e2 }
  | e = expression8a { e }
expression8a:
  | FUTURE; e1 = expression8a { ltl_future e1 }
  | GLOBAL; e1 = expression8a { ltl_global e1 }
  | NEXT; e1 = expression8a { ltl_next e1 }
  | e = expression7 { e }
expression7:
  | e = expression6 { e }
expression6:
  | e1 = expression5; OR; e2 = expression6 { ltl_disj2 e1 e2 }
  | e = expression5 { e }
expression5:
  | e = expression4 { e }
expression4:
  | e1 = expression3; AND; e2 = expression4 { ltl_conj2 e1 e2 }
  | e = expression3 { e }
expression3:
  | e = expression2 { e }
expression2:
  | NOT; e1 = expression2 { ltl_neg e1 }
  | e = expression1 { e }
expression1:
  | id = IDENT { ltl_atom id }
  | LPAREN; e = expression; RPAREN { e }
  | TOP { ltl_top }
  | BOT { ltl_bot }
