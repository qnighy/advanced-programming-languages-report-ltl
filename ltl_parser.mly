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
  | e1 = expression9; EQUIV; e2 = expression9 { LTLequiv (e1, e2) }
  | e1 = expression9; IMPLIES; e2 = expression10 { LTLimpl (e1, e2) }
  | e = expression9 { e }
expression9:
  | e = expression8 { e }
expression8:
  | e1 = expression7; UNTIL; e2 = expression7 { LTLuntil (e1, e2) }
  | e1 = expression7; RELEASE; e2 = expression7 { LTLrelease (e1, e2) }
  | e = expression8a { e }
expression8a:
  | FUTURE; e1 = expression8a { LTLfuture e1 }
  | GLOBAL; e1 = expression8a { LTLglobal e1 }
  | NEXT; e1 = expression8a { LTLnext e1 }
  | e = expression7 { e }
expression7:
  | e = expression6 { e }
expression6:
  | e1 = expression5; OR; e2 = expression6 { LTLconj (e1, e2) }
  | e = expression5 { e }
expression5:
  | e = expression4 { e }
expression4:
  | e1 = expression3; AND; e2 = expression4 { LTLdisj (e1, e2) }
  | e = expression3 { e }
expression3:
  | e = expression2 { e }
expression2:
  | NOT; e1 = expression2 { LTLneg e1 }
  | e = expression1 { e }
expression1:
  | id = IDENT { LTLatom id }
  | LPAREN; e = expression; RPAREN { e }
  | TOP { LTLtop }
  | BOT { LTLbot }
