(*
*)

open Types
open Format
open Automaton
(* open TreeAutomaton *)
open Compile

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

let () =
  if Array.length Sys.argv > 1 then begin
    let c = open_in Sys.argv.(1) in
    let lb = Lexing.from_channel c in
    let tlist =
      try Parser.main Lexer.lexer lb
      with _ ->
        let p = lb.lex_curr_p in
        printf "Error : line %d, column %d" p.pos_lnum (p.pos_cnum - p.pos_bol);
        exit 1 in
    List.iter (fun t -> print_type_def t) tlist;
    List.iter (fun t ->
        let _ = compile_label t.label in ()) tlist;
    let init = new_state () in
    let automaton = compile_regexp init (List.hd tlist).regexp in
    printf "%a@." pp_automaton automaton
  end
