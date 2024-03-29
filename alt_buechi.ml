open Ltl
open Positive_formula

type alt_buechi = {
  alphabet_size : int;
  mutable num_states : int;
  mutable start : int;
  next : (int * int, positive_formula) Hashtbl.t;
  accepts : (int, bool) Hashtbl.t
}

let rec ltl_construct_atomhash atomhash e =
  match e with
  | LTLposatom s | LTLnegatom s ->
      if not (Hashtbl.mem atomhash s) then
        Hashtbl.add atomhash s (Hashtbl.length atomhash)
  | LTLconj el | LTLdisj el -> List.iter (ltl_construct_atomhash atomhash) el
  | LTLnext e0 -> ltl_construct_atomhash atomhash e0
  | LTLuntil (e0, e1) | LTLrelease (e0, e1) ->
      ltl_construct_atomhash atomhash e0;
      ltl_construct_atomhash atomhash e1

let rec alt_buechi_from_ltl_createstate m prophash atomhash e =
  try
    Hashtbl.find prophash e
  with Not_found ->
    let state = m.num_states in
    m.num_states <- m.num_states + 1;
    Hashtbl.add prophash e state;
    for i = 0 to m.alphabet_size - 1 do
      Hashtbl.add m.next (state, i)
        (alt_buechi_from_ltl_createnext m prophash atomhash i e)
    done;
    Hashtbl.add m.accepts state
      (match e with LTLrelease _ -> true | _ -> false);
    state
and alt_buechi_from_ltl_createatom m prophash atomhash e =
  PFatom (alt_buechi_from_ltl_createstate m prophash atomhash e)
and alt_buechi_from_ltl_createnext m prophash atomhash i e =
  match e with
  | LTLposatom s ->
      let sn = Hashtbl.find atomhash s in
      if ((i lsr sn) land 1) == 1 then pf_top else pf_bot
  | LTLnegatom s ->
      let sn = Hashtbl.find atomhash s in
      if ((i lsr sn) land 1) == 1 then pf_bot else pf_top
  | LTLconj el ->
      pf_conj (List.map
        (alt_buechi_from_ltl_createnext m prophash atomhash i) el)
  | LTLdisj el ->
      pf_disj (List.map
        (alt_buechi_from_ltl_createnext m prophash atomhash i) el)
  | LTLnext e0 -> alt_buechi_from_ltl_createatom m prophash atomhash e0
  | LTLuntil (e0, e1) ->
      pf_disj2 (alt_buechi_from_ltl_createnext m prophash atomhash i e1)
        (pf_conj2
          (alt_buechi_from_ltl_createnext m prophash atomhash i e0)
          (alt_buechi_from_ltl_createatom m prophash atomhash e))
  | LTLrelease (e0, e1) ->
      pf_conj2 (alt_buechi_from_ltl_createnext m prophash atomhash i e1)
        (pf_disj2
          (alt_buechi_from_ltl_createnext m prophash atomhash i e0)
          (alt_buechi_from_ltl_createatom m prophash atomhash e))

let alt_buechi_from_ltl e =
  let prophash = Hashtbl.create 54541 in
  let atomhash = Hashtbl.create 996345 in
  ltl_construct_atomhash atomhash e;
  let rev_atomhash = Hashtbl.create 55511 in
  Hashtbl.iter (fun x y -> Hashtbl.add rev_atomhash y x) atomhash;
  let m = {
    alphabet_size = 1 lsl (Hashtbl.length atomhash);
    num_states = 0;
    start = -1;
    next = Hashtbl.create 15442;
    accepts = Hashtbl.create 99543
  } in
  m.start <-
    alt_buechi_from_ltl_createstate m prophash atomhash e;
  (m, atomhash, rev_atomhash)

let pp_alt_buechi atomhash rev_atomhash pf m =
  Format.fprintf pf "@[<hv 2>{";
  Format.fprintf pf "@[alphabet_size@ =@ %d@]@," m.alphabet_size;
  Format.fprintf pf "@[num_states@ =@ %d@]@," m.num_states;
  Format.fprintf pf "@[start@ =@ %d@]@," m.start;
  for fromstate = 0 to m.num_states-1 do
    for propstate = 0 to m.alphabet_size-1 do
      let formula = Hashtbl.find m.next (fromstate, propstate) in
      Format.fprintf pf "@[next(%d,@ @[{" fromstate;
      for i = 0 to Hashtbl.length atomhash - 1 do
        if ((propstate lsr i) land 1) == 1 then
          Format.fprintf pf "%s,@ " (Hashtbl.find rev_atomhash i)
      done;
      Format.fprintf pf "}@])@ =@ %a;@]@," pp_positive_formula formula
    done;
    let accepts = Hashtbl.find m.accepts fromstate in
    Format.fprintf pf "@[accepts(%d)@ =@ %B@]@," fromstate accepts
  done;
  Format.fprintf pf "}@]";
