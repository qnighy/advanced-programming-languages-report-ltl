type ltl =
  | LTLatom of string
  | LTLtop
  | LTLconj of ltl * ltl
  | LTLbot
  | LTLdisj of ltl * ltl
  | LTLneg of ltl
  | LTLimpl of ltl * ltl
  | LTLequiv of ltl * ltl
  | LTLnext of ltl
  | LTLfuture of ltl
  | LTLglobal of ltl
  | LTLuntil of ltl * ltl
  | LTLrelease of ltl * ltl
