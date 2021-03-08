(** Définition des automates d'arbres non déterministes où les étiquettes sont
    des chaînes de caractères.

    - Les états sont des chaînes de caractères.
    - Les transitions sont des map qui associent à un état une liste de triplets
      ensemble d'étiquettes * état du fils gauche * état du fils droit.
      On utilise le module ListMap, qui se trouve dans le module Utils, pour
      implémenter ces maps de listes.
*)

open Format
open Types
open Tree
open Utils

(* Ensemble d'étiquettes pour les transitions. *)
type labelset =
  | Finite of alphabet
  | CoFinite of alphabet

type transition_key = state
type transition_values = labelset * state * state

module TransitionMap = ListMap(String)
type transitions = transition_values TransitionMap.t

type tree_automaton = {
  states: states;
  initial: states;
  final: states;
  transitions: transitions
}


(************************** PRETTY-PRINTERS ************************************)

let pp_transition_map indent f =
  TransitionMap.iter (fun q (label, q1, q2) ->
      match label with
      | Finite s -> fprintf f "%s %a, %s --> %s, %s\n" indent pp_string_set s q q1 q2
      | CoFinite s -> fprintf f "%s CO %a, %s --> %s, %s\n" indent pp_string_set s q q1 q2)

let pp f (a : tree_automaton) =
  let pps = pp_string_set in
  let ppt = pp_transition_map "\t\t" in
  fprintf f "Automaton :
\t States : %a
\t Initial : %a
\t Final : %a
\t Transitions : @[%a@]" pps a.states pps a.initial pps a.final ppt a.transitions


(*********** RECONNAISSANCE D'ARBRES PAR UN AUTOMATE D'ARBRE *******************)

(* Validation Bottom-Up (non utilisée ici). *)
let rec validate_bu (a : tree_automaton) (t : binary_tree_map) (p : string) =
  let tp = StringMap.find p t in
  if tp = "#" then StringSet.singleton "#"
  else
    let r1 = validate_bu a t (p ^ "1") in
    let r2 = validate_bu a t (p ^ "2") in
    TransitionMap.fold (fun q (labels, q1, q2) r ->
            match labels with
            | Finite s ->
              if StringSet.mem q1 r1 && StringSet.mem q2 r2 && StringSet.mem tp s then
                StringSet.add q r
              else r
            | CoFinite s ->
              if StringSet.mem q1 r1 && StringSet.mem q2 r2 && not (StringSet.mem tp s) then
                StringSet.add q r
              else r
      ) a.transitions StringSet.empty

(* Validation Bottom-Up optimisée.
   Arguments :
   - a : un automate d'arbre
   - t : un arbre binaire représenté par un map associant à chaque chemin une
         étiquette
   - p : un chemin
   - c : un ensemble d'états candidats
   Renvoie un sous-ensemble d'états r de c tel que tous les états de r acceptent
   le sous-arbre de t enregicé en p.
*)
let rec validate_opt (a : tree_automaton) (t: binary_tree_map) (p : string) (c : states) =
  let tp = StringMap.find p t in
  if StringSet.is_empty c then StringSet.empty
  else if StringSet.mem "#" c then StringSet.singleton "#"
  else
    let c', c1, c2 =
      TransitionMap.fold (fun q (labels, q1, q2) (c', c1, c2) ->
          match labels with
          | Finite s ->
            if StringSet.mem q c && StringSet.mem tp s then
              (StringSet.add q c', StringSet.add q1 c1, StringSet.add q2 c2)
            else
              (c', c1, c2)
          | CoFinite s ->
            if StringSet.mem q c && not (StringSet.mem tp s) then
              (StringSet.add q c', StringSet.add q1 c1, StringSet.add q2 c2)
            else
              (c', c1, c2)
        ) a.transitions (StringSet.empty, StringSet.empty, StringSet.empty) in
    let r1 = validate_opt a t (p ^ "1") c1 in
    if StringSet.is_empty r1 then StringSet.empty
    else
      let r2 = validate_opt a t (p ^ "2") c2 in
      if StringSet.is_empty r2 then StringSet.empty
      else
        TransitionMap.fold (fun q (_, q1, q2) r ->
            if StringSet.mem q c' && StringSet.mem q1 r1 && StringSet.mem q2 r2 then
              StringSet.add q r
            else r
          ) a.transitions StringSet.empty
