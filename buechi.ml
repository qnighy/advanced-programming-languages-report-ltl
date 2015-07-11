open Ltl
open Positive_formula
open Alt_buechi

type buechi = {
  alphabet_size : int;
  mutable num_states : int;
  mutable start : int list;
  next : (int * int, int list) Hashtbl.t;
  accepts : (int, bool) Hashtbl.t
}

let rec make_bits = function
  | [] -> 0
  | h :: l -> (1 lsl h) lor make_bits l

let rec buechi_from_alt_createstate
    (am : alt_buechi) (m : buechi) powerhash fbits u v =
  try
    Hashtbl.find powerhash (u, v)
  with Not_found ->
    let state = m.num_states in
    m.num_states <- m.num_states + 1;
    Hashtbl.add powerhash (u, v) state;
    Hashtbl.add m.accepts state (u == 0);
    let (u2, v2) = if u == 0 then (v, 0) else (u, v) in
    for i = 0 to m.alphabet_size - 1 do
      let xs = ref pf_top in
      for j = 0 to am.num_states - 1 do
        if ((u2 lsr j) land 1) == 1 then
          xs := pf_conj2 (!xs) (Hashtbl.find am.next (j, i))
      done;
      let xs = List.map make_bits (positive_dnf (!xs)) in

      let ys = ref pf_top in
      for j = 0 to am.num_states - 1 do
        if ((v2 lsr j) land 1) == 1 then
          ys := pf_conj2 (!ys) (Hashtbl.find am.next (j, i))
      done;
      let ys = List.map make_bits (positive_dnf (!ys)) in

      let next =
        List.concat (
          List.map (fun x ->
            List.map (fun y ->
              buechi_from_alt_createstate am m powerhash fbits
                (x land (lnot fbits)) (y lor (x land fbits))) ys) xs) in
      Hashtbl.add m.next (state, i) next
    done;
    state

let buechi_from_alt (am : alt_buechi) =
  let powerhash = Hashtbl.create 77225 in
  let fbits = ref 0 in
  for i = 0 to am.num_states - 1 do
    if Hashtbl.find am.accepts i then
      fbits := (!fbits) lor (1 lsl i)
  done;
  let fbits = !fbits in
  let m : buechi = {
    alphabet_size = am.alphabet_size;
    num_states = 0;
    start = [];
    next = Hashtbl.create 94438;
    accepts = Hashtbl.create 28287
  } in
  m.start <- [
    buechi_from_alt_createstate am m powerhash fbits (1 lsl am.start) 0];
  m

let pp_buechi atomhash rev_atomhash pf (m : buechi) =
  Format.printf "@[<hv 2>{";
  Format.printf "@[alphabet_size@ =@ %d@]@," m.alphabet_size;
  Format.printf "@[num_states@ =@ %d@]@," m.num_states;
  Format.printf "@[start@ =@ [";
  List.iter (Format.printf "%d,@ ") m.start;
  Format.printf "]@]@,";
  for fromstate = 0 to m.num_states-1 do
    for propstate = 0 to m.alphabet_size-1 do
      let next = Hashtbl.find m.next (fromstate, propstate) in
      Format.printf "@[next(%d,@ @[{" fromstate;
      for i = 0 to Hashtbl.length atomhash - 1 do
        if ((propstate lsr i) land 1) == 1 then
          Format.printf "%s,@ " (Hashtbl.find rev_atomhash i)
      done;
      Format.printf "}@])@ =@ [";
      List.iter (Format.printf "%d,@ ") next;
      Format.printf "];@]@,"
    done;
    let accepts = Hashtbl.find m.accepts fromstate in
    Format.printf "@[accepts(%d)@ =@ %B@]@," fromstate accepts
  done;
  Format.printf "}@]";
