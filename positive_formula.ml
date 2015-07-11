type positive_formula =
  | PFatom of int
  | PFconj of positive_formula list
  | PFdisj of positive_formula list

let pf_top = PFconj []
let pf_bot = PFdisj []
let pf_elim e =
  match e with
  | PFdisj [e0] | PFconj [e0] -> e0
  | _ -> e
let pf_conj2 e0 e1 = pf_elim
  begin match e0, e1 with
  | PFdisj [], _ | _, PFdisj [] -> PFdisj []
  | PFconj e0l, PFconj e1l -> PFconj (e0l @ e1l)
  | PFconj e0l, _ -> PFconj (e0l @ [e1])
  | _, PFconj e1l -> PFconj (e0 :: e1l)
  | _, _ -> PFconj [e0; e1]
  end
let pf_disj2 e0 e1 = pf_elim
  begin match e0, e1 with
  | PFconj [], _ | _, PFconj [] -> PFconj []
  | PFdisj e0l, PFdisj e1l -> PFdisj (e0l @ e1l)
  | PFdisj e0l, _ -> PFdisj (e0l @ [e1])
  | _, PFdisj e1l -> PFdisj (e0 :: e1l)
  | _, _ -> PFdisj [e0; e1]
  end

let pf_conj el = List.fold_right pf_conj2 el pf_top
let pf_disj el = List.fold_right pf_disj2 el pf_bot

let rec pp_positive_formula pf = function
  | PFatom x -> Format.printf "@[[%d]@]" x
  | PFconj [] -> Format.printf "@[True@]"
  | PFconj (h::t) ->
      Format.printf "@[(%a" pp_positive_formula h;
      List.iter (fun x ->
        Format.printf "@ /\\@ %a" pp_positive_formula x) t;
      Format.printf ")@]"
  | PFdisj [] -> Format.printf "@[False@]"
  | PFdisj (h::t) ->
      Format.printf "@[(%a" pp_positive_formula h;
      List.iter (fun x ->
        Format.printf "@ \\/@ %a" pp_positive_formula x) t;
      Format.printf ")@]"
