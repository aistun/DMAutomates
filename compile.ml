open Types
open TreeAutomaton

(* Redéfinitions de modules et types pour simplifier l'écriture du code
   plus loin. *)
module TreeATr = TreeAutomaton.TransitionMap
module ATr = Automaton.TransitionMap

type automaton = Automaton.automaton
type tree_automaton = TreeAutomaton.tree_automaton

(* Identifiant unique pour les états. *)
let new_state, reset_state =
  let n = ref 0 in
  (fun () ->
    incr n;
    string_of_int !n),
  (fun () -> n := 0)

(* Identifiant unique pour la linéarisation des expressions régulières. *)
let new_int, reset_int =
  let n = ref 0 in
  (fun () ->
    incr n;
    !n),
  (fun () -> n := 0)

(* Compilation de la définition de l'étiquette d'un type vers un ensemble fini
   ou co-fini d'étiquettes. *)
let compile_label = function
  | Ident s  -> Finite (StringSet.singleton s)
  | Any      -> CoFinite StringSet.empty
  | AllBut l -> CoFinite (StringSet.of_list l)

(* Linéarisation d'une expression régulière. *)
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

(* Renvoie true si l'expression régulière permet de reconnaître epsilon. *)
let rec recognize_epsilon = function
  | LQMark _ | LStar _ -> true
  | LConcat(r1, _) -> recognize_epsilon r1
  | LAlt(r1, r2) -> recognize_epsilon r1 || recognize_epsilon r2
  | _ -> false

(* Premiers éléments atomiques (= symboles = chaînes de caractères) possibles
   dans les mots reconnus par l'expression régulière. *)
let rec first = function
  | LEmpty      -> IntSet.empty
  | LAtom(_, i) -> IntSet.singleton i
  | LStar r     -> first r
  | LQMark r    -> first r
  | LConcat(r1, r2) ->
    if recognize_epsilon r1 then IntSet.union (first r1) (first r2)
    else first r1
  | LAlt(r1, r2)    -> IntSet.union (first r1) (first r2)

(* Derniers éléments atomiques possibles dans les mots reconnus par
   l'expression régulière. *)
let rec last = function
  | LEmpty      -> IntSet.empty
  | LAtom(_, i) -> IntSet.singleton i
  | LStar r     -> last r
  | LQMark r    -> last r
  | LConcat(r1, r2) ->
    if recognize_epsilon r2 then IntSet.union (last r1) (last r2)
    else last r2
  | LAlt(r1, r2)    -> IntSet.union (last r1) (last r2)

(* Produit cartésien de deux ensembles d'entiers. *)
let prod s1 s2 =
  IntSet.fold (fun x1 s ->
      IntSet.fold (fun x2 s ->
          IntIntSet.add (x1, x2) s
        ) s2 s) s1 IntIntSet.empty

(* Facteurs de longueur 2 dans les mots reconnus par une expression régulière. *)
let rec factors = function
  | LEmpty   -> IntIntSet.empty
  | LAtom _  -> IntIntSet.empty
  | LStar r  -> IntIntSet.union (factors r) (prod (last r) (first r))
  | LQMark r -> factors r
  | LConcat(r1, r2) -> IntIntSet.union (factors r1)
                         (IntIntSet.union (factors r2)
                            (prod (last r1) (first r2)))
  | LAlt(r1, r2) -> IntIntSet.union (factors r1) (factors r2)

(* Nombre d'états dans l'automate construit par la construction de Glushkov à
   partir de l'expression régulière. Il s'agit en fait du nombre d'éléments
   atomiques dans l'expression régulière. *)
let rec nb_states = function
  | LEmpty   -> 0
  | LAtom _  -> 1
  | LStar r
  | LQMark r -> nb_states r
  | LConcat(r1, r2)
  | LAlt(r1, r2)    -> nb_states r1 + nb_states r2

(* Construction du NFA correspondant à l'expression régulière r et ayant
   pour unique état initial `init`.
   J'utilise ici la constructon de Glushkov. *)
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
      ATr.add key states_arr.(x) tr
    ) f ATr.empty in
  let transitions = IntIntSet.fold (fun (x1, x2) tr ->
      let key = states_arr.(x1), IntMap.find x2 back in
      ATr.add key states_arr.(x2) tr
    ) fac initial_transitions in
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
  } : automaton)

(* Supprime les états inaccessibles de l'automate d'arbre. *)
let clear states transitions =
  (* remove : l'ensemble des états inaccessibles donc à supprimer *)
  let remove =
    TreeATr.fold_map (fun k _ remove ->
        StringSet.remove k remove) transitions (StringSet.remove "#" states) in
  let transitions' =
    TreeATr.filter (fun _ (_, qd, qr) ->
        not (StringSet.mem qd remove || StringSet.mem qr remove)
      ) transitions in
  StringSet.diff states remove, transitions'

(* Compile une liste de définitions de types en automate d'arbre. *)
let compile_types tlist init =
  (* Création d'états pour chaque type et compilation des étiquettes associées
     à ces types.
     q : l'ensemble des états, un pour chaque type
     qlmap  : map qui associe à chaque nom de type une liste composé d'états lui
              correspondant associé à l'étiquette compilée.
              En effet, si dans le fichier de types on a :
                  type t = L[ f ]
                  type t = N[ g ]
              Alors, la liste associée à t dans qlmap est par exemple :
                  [(ql1, Finite {f}); (ql2, Finite {g})]
     tlist' : une version modifiée de tlist où on a changé les id des
              définitions de types en des identifiants uniques correspondant
              à des états.
              Dans l'exemple précédent, l'id de la première définition de type
              est changée à qt1 et l'id de la deuxième est changé à qt2) *)
  let q, qlmap, tlist' = List.fold_left (fun (q, qlmap, tlist') t ->
      let qt = "q" ^ t.id ^ new_state () in
      let lt = compile_label t.label in
      StringSet.add qt q,
      StringListMap.add t.id (qt, lt) qlmap,
      {id = qt; label = t.label; regexp = t.regexp} :: tlist'
    ) (StringSet.empty, StringListMap.empty, []) tlist in
  reset_state (); (* réinitialise le compteur des états *)
  (* Correction *)
  let q, qlmap = List.fold_left (fun (q, qlmap) t ->
      StringListMap.fold_key (fun (qt, lt) (q, qlmap) ->
          if t.regexp = Empty then
            (StringSet.remove qt q, StringListMap.add t.id ("#", lt) qlmap)
          else
            (q, qlmap)
        ) t.id qlmap (q, qlmap)
    ) (q, qlmap) tlist in
  let q', tr = List.fold_left (fun (q, tr) t ->
      let a = compile_regexp t.id t.regexp in
      let tr =
        ATr.fold (fun (u, m) v tr ->
            StringListMap.fold_key (fun (qt, label) tr ->
                TreeATr.add_list
                  u
                  ((label, qt, v) ::
                   if StringSet.mem v a.final then [(label, qt, "#")] else [])
                  tr
              ) m qlmap tr
          ) a.transitions tr in
      (StringSet.union a.states q, tr)
    ) (q, TreeATr.empty) tlist' in
  let qlinit = StringListMap.find init qlmap in
  let states, transitions = clear q' tr in
  let _ = init in
  {
    states = StringSet.add "accept" (StringSet.add "#" states);
    initial = StringSet.singleton "accept";
    final = StringSet.empty;
    transitions = TreeATr.add_list
        "accept"
        (List.map (fun (qi, label) -> (label, qi, "#")) qlinit)
        transitions
  }
