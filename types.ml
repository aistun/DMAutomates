open Format

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

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type alphabet = StringSet.t
type state  = string
type states = StringSet.t


(* module MapList(Ord : OrderedType) : sig
 *   type 'a t
 *   let add :  *)


let rec pp_alt f = function
  | Alt(r1, r2) -> fprintf f "%a | %a" pp_alt r1 pp_alt r2
  | _ as r     -> fprintf f "%a" pp_concat r
and pp_concat f = function
  | Concat(r1, r2) -> fprintf f "%a %a" pp_concat r1 pp_concat r2
  | _ as r         -> fprintf f "%a" pp_other_regexp r
and pp_other_regexp f = function
  | Empty          -> ()
  | Atom s         -> fprintf f "%s" s
  | Star (Atom s)  -> fprintf f "%s*" s
  | Star r         -> fprintf f "(%a)*" pp_other_regexp r
  | QMark (Atom s) -> fprintf f "%s?" s
  | QMark r        -> fprintf f "(%a)?" pp_other_regexp r
  | _ as r         -> fprintf f "(%a)" pp_alt r

let pp_label f = function
  | Ident s  -> fprintf f "%s" s
  | Any      -> fprintf f "*"
  | AllBut l -> List.iter (fun x -> fprintf f "~%s" x) l

let pp_tdef f t =
  fprintf f "type %s = %a[ %a ]" t.id pp_label t.label pp_alt t.regexp

let print_type_regexp = printf "%a@." pp_alt
let print_type_def = printf "%a@." pp_tdef
