(** Différentes définitions de types liées aux types d'arbres définis dans les
    fichiers *.typ .
    Ainsi, que plusieurs modules utiles par la suite à différents endroits du
    code.
*)

open Format
open Utils

(* Types des expressions régluières avec pour éléments atomiques des chaînes
   de caractères. *)
type regexp =
  | Empty
  | Atom   of string
  | Star   of regexp
  | QMark  of regexp
  | Concat of regexp * regexp
  | Alt    of regexp * regexp

(* Type semblable à regexp mais avec entier supplémentaire pour le constructeur
   LAtom. Ce type sert à représenter l'expression régulière linéarisée dans
   la construction de Glushkov. *)
type linear_regexp =
  | LEmpty
  | LAtom   of string * int
  | LStar   of linear_regexp
  | LQMark  of linear_regexp
  | LConcat of linear_regexp * linear_regexp
  | LAlt    of linear_regexp * linear_regexp

(* Étiquettes de type d'arbre. *)
type label =
  | Ident of string
  | Any
  | AllBut of string list

(* Type représentant une définition de type d'arbre telle qu'elle sera lu
   par le parser depuis les fichiers *.typ . *)
type type_def = {
  id:     string;
  label:  label;
  regexp: regexp
}

(* Quelques modules utiles. *)
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

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
module StringListMap = ListMap(String)

(* Types communs liés aux automates de mots et automates d'arbres. *)
type alphabet = StringSet.t
type state  = string
type states = StringSet.t

(************************** PRETTY-PRINTERS ************************************)

let pp_string_set f s =
  if StringSet.cardinal s = 0 then fprintf f "{}"
  else
    StringSet.iter (fun x -> fprintf f "%s " x) s

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
