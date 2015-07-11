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

let rec clause_impl (cl1 : int list) (cl2 : int list) =
  match cl1, cl2 with
  | _, [] -> true
  | _, (cl2h :: cl2t) ->
      List.mem cl2h cl1 && clause_impl cl1 cl2t

let rec cleanup_dnf = function
  | [] -> []
  | h :: t ->
      if List.exists (fun t0 -> clause_impl h t0) t then
        cleanup_dnf t
      else h :: cleanup_dnf t

let rec myuniq = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: z when x == y -> x :: myuniq z
  | x :: y -> x :: myuniq y

let rec positive_dnf = function
  | PFatom x -> [[x]]
  | PFconj el ->
      List.fold_right
        (fun dnf1 dnf2 ->
          cleanup_dnf (
            List.concat (List.map (fun cl1 -> List.map (fun cl2 ->
              myuniq (List.merge compare cl1 cl2)) dnf2) dnf1)))
        (List.map positive_dnf el) [[]]
  | PFdisj el ->
      cleanup_dnf (List.concat (List.map positive_dnf el))
