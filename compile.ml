open Types
open TreeAutomaton

(* inutile finalement car les exclusions d'étiquettes sont des chaines de
 * de caractères *)
let fusion l =
  List.fold_left (fun (finite, cofinite) x ->
      match finite, cofinite, x with
      | None, s, (Finite _ as sf) ->
        (Some sf, s)
      | s, None, (CoFinite _ as scof) ->
        (s, Some scof)
      | Some (Finite ef), s, Finite e ->
        Some (Finite (StringSet.union ef e)), s
      | s, Some (CoFinite ecof), CoFinite e ->
        s, Some (CoFinite (StringSet.inter ecof e))
      | _ -> assert false) (None, None) l

let compile_label = function
  | Ident s  -> Finite (StringSet.singleton s)
  | Any      -> CoFinite StringSet.empty
  | AllBut l -> CoFinite (StringSet.of_list l)

let new_state, reset_state =
  let n = ref 0 in
  (fun () ->
    incr n;
    string_of_int !n),
  (fun () -> n := 0)

let new_int, reset_int =
  let n = ref 0 in
  (fun () ->
    incr n;
    !n),
  (fun () -> n := 0)

module IntSet = Set.Make(struct
    type t = int
    let compare = compare
  end)

module IntMap = Map.Make(struct
    type t = int
    let compare = compare
  end)

module IntInt = struct
  type t = int * int
  let compare (a1, a2) (b1, b2) =
    if a1 < b1 then -1
    else if a1 = b1 && a2 < b2 then -1
    else if a1 = b1 && a2 = b2 then 0
    else 1
end

module IntIntSet = Set.Make(IntInt)

let linear_regexp_of_regexp r =
  let rec aux back = function
    | Empty -> back, LEmpty
    | Atom s ->
      let i = new_int () in
      IntMap.add i s back, LAtom (s, i)
    | Star r ->
      let back', r' = aux back r in
      back', LStar r'
    | QMark r ->
      let back', r' = aux back r in
      back', LQMark r'
    | Concat(r1, r2) ->
      let back', r1' = aux back r1 in
      let back'', r2' = aux back' r2 in
      back'', LConcat(r1', r2')
    | Alt(r1, r2) ->
      let back', r1' = aux back r1 in
      let back'', r2' = aux back' r2 in
      back'', LAlt(r1', r2') in
  reset_int ();
  aux IntMap.empty r

let rec recognize_epsilon = function
  | LQMark _ | LStar _ -> true
  | LConcat(r1, _) -> recognize_epsilon r1
  | LAlt(r1, r2) -> recognize_epsilon r1 || recognize_epsilon r2
  | _ -> false

let rec first = function
  | LEmpty      -> IntSet.empty
  | LAtom(_, i) -> IntSet.singleton i
  | LStar r     -> first r
  | LQMark r    -> first r
  | LConcat(r1, r2) ->
    if recognize_epsilon r1 then IntSet.union (first r1) (first r2)
    else first r1
  | LAlt(r1, r2)    -> IntSet.union (first r1) (first r2)

let rec last = function
  | LEmpty      -> IntSet.empty
  | LAtom(_, i) -> IntSet.singleton i
  | LStar r     -> last r
  | LQMark r    -> last r
  | LConcat(r1, r2) ->
    if recognize_epsilon r2 then IntSet.union (last r1) (last r2)
    else last r2
  | LAlt(r1, r2)    -> IntSet.union (last r1) (last r2)

let prod s1 s2 =
  IntSet.fold (fun x1 s ->
      IntSet.fold (fun x2 s ->
          IntIntSet.add (x1, x2) s
        ) s2 s) s1 IntIntSet.empty

let rec factors = function
  | LEmpty   -> IntIntSet.empty
  | LAtom _  -> IntIntSet.empty
  | LStar r  -> IntIntSet.union (factors r) (prod (last r) (first r))
  | LQMark r -> factors r
  | LConcat(r1, r2) -> IntIntSet.union (factors r1)
                        (IntIntSet.union (factors r2)
                           (prod (last r1) (first r2)))
  | LAlt(r1, r2) -> IntIntSet.union (factors r1) (factors r2)

let rec nb_states = function
  | LEmpty   -> 0
  | LAtom _  -> 1
  | LStar r
  | LQMark r -> nb_states r
  | LConcat(r1, r2)
  | LAlt(r1, r2)    -> nb_states r1 + nb_states r2

(**
 * Construction du NFA correspondant à l'expression régulière r et ayant
 * pour unique état initial `init`.
 * J'utilise ici la constructon de Glushkov.
 *)
let compile_regexp init r =
  let back, lr = linear_regexp_of_regexp r in
  let n = nb_states lr in
  let states_arr =
    Array.init (n + 1) (fun i -> if i = 0 then init else new_state ()) in
  let f   = first lr in
  let l   = last  lr in
  let fac = factors lr in
  let initial_transitions = IntSet.fold (fun x tr ->
      let key = init, IntMap.find x back in
      Automaton.TransitionMap.add
        key
        (states_arr.(x) ::
         (try Automaton.TransitionMap.find key tr with Not_found -> []))
        tr)
      f Automaton.TransitionMap.empty in
  let transitions = IntIntSet.fold (fun (x1, x2) tr ->
      let key = states_arr.(x1), IntMap.find x2 back in
      Automaton.TransitionMap.add
        key
        (states_arr.(x2) ::
         (try Automaton.TransitionMap.find key tr with Not_found -> []))
        tr)
      fac initial_transitions in
  let initial = StringSet.singleton init in
  let final   = IntSet.fold (fun x s ->
      StringSet.add states_arr.(x) s) l StringSet.empty in
  let states  = Array.fold_left (fun s x ->
      StringSet.add x s) StringSet.empty states_arr in
  ({
    states = states;
    initial = initial;
    final   = final;
    transitions = transitions
  } : Automaton.automaton)

module StringMap = Map.Make(String)

let clear states transitions =
  let remove =
    TreeAutomaton.TransitionMap.fold (fun k _ remove ->
        StringSet.remove k remove) transitions (StringSet.remove "#" states) in
  let transitions' =
    TreeAutomaton.TransitionMap.map (fun lst ->
        List.filter (fun (_, qd, qr) ->
            not (StringSet.mem qd remove || StringSet.mem qr remove)) lst) transitions in
  let transitions'' =
    TreeAutomaton.TransitionMap.filter (fun _ lst -> lst <> []) transitions' in
  StringSet.diff states remove, transitions''

let compile_types tlist init =
  let q, qmap, lmap = List.fold_left (fun (q, qmap, lmap) t ->
      let qt = "q" ^ t.id in
      let lt = compile_label t.label in
      StringSet.add qt q, StringMap.add t.id qt qmap, StringMap.add t.id (lt :: (try StringMap.find t.id lmap with Not_found -> [])) lmap
    ) (StringSet.empty, StringMap.empty, StringMap.empty) tlist in
  let q, qmap = List.fold_left (fun (q, qmap) t ->
      let qt = StringMap.find t.id qmap in
      if t.regexp = Empty then
        (StringSet.remove qt q, StringMap.add t.id "#" qmap)
      else
        (q, qmap)
    ) (q, qmap) tlist in
  let q', tr = List.fold_left (fun (q, tr) t ->
      let a = compile_regexp (StringMap.find t.id qmap) t.regexp in
      let tr =
        Automaton.TransitionMap.fold (fun (u, m) vl tr ->
            List.fold_left (fun tr v ->
                List.fold_left (fun tr (label : labelset) ->
                    let qt    = StringMap.find m qmap in
                    let lst   =
                      try TreeAutomaton.TransitionMap.find u tr with Not_found -> [] in
                    TreeAutomaton.TransitionMap.add
                      u
                      ((label, qt, v) ::
                       if StringSet.mem v a.final then (label, qt, "#") :: lst
                       else lst)
                      tr
                  ) tr (StringMap.find m lmap)
              ) tr vl
          ) a.transitions tr in
      (StringSet.union a.states q, tr)
    ) (q, TreeAutomaton.TransitionMap.empty) tlist in
  let qinit = StringMap.find init qmap in
  let linit = StringMap.find init lmap in
  let states, transitions = clear q' tr in
  {
    states = StringSet.add "accept" (StringSet.add "#" states);
    initial = StringSet.singleton "accept";
    final = StringSet.empty;
    transitions =
      TreeAutomaton.TransitionMap.add "accept" (List.map (fun label -> (label, qinit, "#")) linit) transitions
  }
