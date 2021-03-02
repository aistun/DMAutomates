open Format

module StringSet = Set.Make(String)

type alphabet = StringSet.t
type state  = string
type states = StringSet.t

type labelset =
  | Finite of alphabet
  | CoFinite of alphabet

type transition_key = state
type transition_values = labelset * state * state

module TransitionMap = Map.Make(String)
type transitions = (transition_values list) TransitionMap.t

type tree_automaton = {
  states: states;
  initial: states;
  final: states;
  transitions: transitions
}

let pp_string_set f s =
  if StringSet.cardinal s = 0 then fprintf f "{}"
  else
    StringSet.iter (fun x -> fprintf f "%s " x) s

let pp_transition_list indent q f =
  List.iter (fun (label, q1, q2) ->
      match label with
      | Finite s -> fprintf f "%s %a, %s --> %s, %s\n" indent pp_string_set s q q1 q2
      | CoFinite s -> fprintf f "%s CO %a, %s --> %s, %s\n" indent pp_string_set s q q1 q2)

let pp_transition_map indent f =
  TransitionMap.iter (fun q q'list -> fprintf f "%a" (pp_transition_list indent q) q'list)

let pp f (a : tree_automaton) =
  let pps = pp_string_set in
  let ppt = pp_transition_map "\t\t" in
  fprintf f "Automaton :
\t States : %a
\t Initial : %a
\t Final : %a
\t Transitions : @[%a@]" pps a.states pps a.initial pps a.final ppt a.transitions
