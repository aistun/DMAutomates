type regexp =
  | Empty
  | Atom   of string
  | Star   of regexp
  | QMark  of regexp
  | Concat of regexp * regexp
  | Alt    of regexp * regexp

type linear_regexp =
  | LEmpty
  | LAtom   of string * int
  | LStar   of linear_regexp
  | LQMark  of linear_regexp
  | LConcat of linear_regexp * linear_regexp
  | LAlt    of linear_regexp * linear_regexp

type label =
  | Ident of string
  | Any
  | AllBut of string list

type type_def = {
  id:     string;
  label:  label;
  regexp: regexp
}

