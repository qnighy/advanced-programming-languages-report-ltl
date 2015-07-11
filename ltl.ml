type ltl =
  | LTLposatom of string
  | LTLnegatom of string
  | LTLconj of ltl list
  | LTLdisj of ltl list
  | LTLnext of ltl
  | LTLuntil of ltl * ltl
  | LTLrelease of ltl * ltl

let ltl_cache = Hashtbl.create 1234

let ltl_intern a =
  try
    Hashtbl.find ltl_cache a
  with Not_found -> (Hashtbl.add ltl_cache a a; a)

let ltl_atom s = ltl_intern (LTLposatom s)

let ltl_top = ltl_intern (LTLconj [])
let ltl_bot = ltl_intern (LTLdisj [])
let ltl_conj2 a b = ltl_intern
  begin match a, b with
  | LTLconj al, LTLconj bl -> LTLconj (al @ bl)
  | LTLconj al, _ -> LTLconj (al @ [b])
  | _, LTLconj bl -> LTLconj (a :: bl)
  | _, _ -> LTLconj [a; b]
  end
let ltl_disj2 a b = ltl_intern
  begin match a, b with
  | LTLdisj al, LTLdisj bl -> LTLdisj (al @ bl)
  | LTLdisj al, _ -> LTLdisj (al @ [b])
  | _, LTLdisj bl -> LTLdisj (a :: bl)
  | _, _ -> LTLdisj [a; b]
  end
let ltl_next a = ltl_intern (LTLnext a)
let ltl_future a = ltl_intern (LTLuntil (ltl_top, a))
let ltl_global a = ltl_intern (LTLrelease (ltl_bot, a))
let ltl_until a b = ltl_intern (LTLuntil (a, b))
let ltl_release a b = ltl_intern (LTLrelease (a, b))

let rec ltl_neg a = ltl_intern
  begin match a with
  | LTLposatom s -> LTLnegatom s
  | LTLnegatom s -> LTLposatom s
  | LTLconj l -> LTLdisj (List.map ltl_neg l)
  | LTLdisj l -> LTLconj (List.map ltl_neg l)
  | LTLnext a -> LTLnext (ltl_neg a)
  | LTLuntil (a, b) -> LTLrelease (ltl_neg a, ltl_neg b)
  | LTLrelease (a, b) -> LTLuntil (ltl_neg a, ltl_neg b)
  end

let ltl_impl a b = ltl_disj2 (ltl_neg a) b
let ltl_equiv a b = ltl_conj2 (ltl_impl a b) (ltl_impl b a)

let rec pp_ltl pf = function
  | LTLposatom s -> Format.fprintf pf "@[%s@]" s
  | LTLnegatom s -> Format.fprintf pf "@[~@[%s@]@]" s
  | LTLconj [] -> Format.fprintf pf "@[True@]"
  | LTLconj (h::t) ->
      Format.fprintf pf "@[(%a" pp_ltl h;
      List.iter (fun x ->
        Format.fprintf pf "@ /\\@ %a" pp_ltl x) t;
      Format.fprintf pf ")@]"
  | LTLdisj [] -> Format.fprintf pf "@[False@]"
  | LTLdisj (h::t) ->
      Format.fprintf pf "@[(%a" pp_ltl h;
      List.iter (fun x ->
        Format.fprintf pf "@ \\/@ %a" pp_ltl x) t;
      Format.fprintf pf ")@]"
  | LTLnext a ->
      Format.fprintf pf "@[(Next@ %a)@]" pp_ltl a
  | LTLuntil (a, b) ->
      Format.fprintf pf "@[(%a@ Until@ %a)@]" pp_ltl a pp_ltl b
  | LTLrelease (a, b) ->
      Format.fprintf pf "@[(%a@ Release@ %a)@]" pp_ltl a pp_ltl b
