(*
*)

open Types
open Format
(* open Automaton
 * open TreeAutomaton *)
open Compile
open Tree

let () =
  if Array.length Sys.argv > 2 then begin
    let c = open_in Sys.argv.(1) in
    let lb = Lexing.from_channel c in
    let tlist =
      try Parser.main Lexer.lexer lb
      with _ ->
        let p = lb.lex_curr_p in
        printf "Error : line %d, column %d" p.pos_lnum (p.pos_cnum - p.pos_bol);
        exit 1 in
    List.iter (fun t -> print_type_def t) tlist;
    printf "@.";
    List.iter (fun t ->
        let _ = compile_label t.label in ()) tlist;
    let ta = compile_types tlist (List.hd tlist).id in
    printf "%a@." TreeAutomaton.pp ta;
    let t  = load_tree Sys.argv.(2) in
    printf "%a@." Tree.pp t;
    let tmap = map_of_binary_tree t in
    let r_bu = TreeAutomaton.validate_bu ta tmap "" in
    printf "%a@." TreeAutomaton.pp_string_set r_bu;
    let r_opt = TreeAutomaton.validate_opt ta tmap "" ta.initial in
    printf "%a@." TreeAutomaton.pp_string_set r_opt
  end
