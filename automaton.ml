(** Définition des automates de mots non déterministes où les symboles sont
    des chaînes de caractères.

    - Les états sont des chaînes de caractères.
    - Les transitions sont des map qui associent à un état et une chaîne de
      caractères une liste d'états. On utilise le module ListMap, qui se trouve
      dans le module Utils, pour implémenter ces maps de listes.
*)

open Format
open Types
open Utils

type transition_key = state * string
type transition_values = state

(* Module implémentant OrderedType pour le type transition_key *)
module TransitionKey = struct
  type t = transition_key
  let compare (a1, a2) (b1, b2) =
    if a1 < b1 then -1
    else if a1 = b1 && a2 < b2 then -1
    else if a1 = b1 && a2 = b2 then 0
    else 1
end

module TransitionMap = ListMap(TransitionKey)
type transitions = transition_values TransitionMap.t

type automaton = {
  states: states;
  initial: states;
  final: states;
  transitions: transitions
}


(************************** PRETTY-PRINTERS ************************************)

let pp_transition_map indent f =
  TransitionMap.iter (fun (q, a) x ->
      fprintf f "%s%s -%s-> %s\n" indent q a x)

let pp f a =
  let pps = pp_string_set in
  let ppt = pp_transition_map "\t\t" in
  fprintf f "Automaton :
\t States : %a
\t Initial : %a
\t Final : %a
\t Transitions : @[%a@]" pps a.states pps a.initial pps a.final ppt a.transitions
