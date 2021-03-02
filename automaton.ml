open Format

module StringSet = Set.Make(String)

type alphabet = StringSet.t
type state  = string
type states = StringSet.t

type transition_key = state * string
type transition_values = state

module TransitionKey = struct
  type t = transition_key
  let compare (a1, a2) (b1, b2) =
    if a1 < b1 then -1
    else if a1 = b1 && a2 < b2 then -1
    else if a1 = b1 && a2 = b2 then 0
    else 1
end

module TransitionMap = Map.Make(TransitionKey)
type transitions = (transition_values list) TransitionMap.t

type automaton = {
  states: states;
  initial: states;
  final: states;
  transitions: transitions
}

let pp_string_set f =
  StringSet.iter (fun x -> fprintf f "%s " x)

let pp_transition_list indent q a f =
  List.iter (fun x -> fprintf f "%s%s -%s-> %s\n" indent q a x)

let pp_transition_map indent f =
  TransitionMap.iter (fun (q, a) q'list -> fprintf f "%a" (pp_transition_list indent q a) q'list)

let pp_automaton f a =
  let pps = pp_string_set in
  let ppt = pp_transition_map "\t\t" in
  fprintf f "Automaton :
\t States : %a
\t Initial : %a
\t Final : %a
\t Transitions : @[%a@]" pps a.states pps a.initial pps a.final ppt a.transitions
