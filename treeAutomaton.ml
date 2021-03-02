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
