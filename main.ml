open Types
open Format
open Compile
open Tree

let print_help f () =
  fprintf f "FORMAT D'APPEL :\n\t validate fichier_type fichier_arbre type_de_la_racine@."

let () =
  if Array.length Sys.argv = 4 then begin
    let c = open_in Sys.argv.(1) in
    let lb = Lexing.from_channel c in
    let tlist =
      try Parser.main Lexer.lexer lb
      with _ ->
        let p = lb.lex_curr_p in
        printf "Error : line %d, column %d" p.pos_lnum (p.pos_cnum - p.pos_bol);
        exit 1 in
    (* List.iter (fun t -> print_type_def t) tlist;
     * printf "@."; *)
    List.iter (fun t ->
        let _ = compile_label t.label in ()) tlist;
    let ta = compile_types tlist Sys.argv.(3) in
    (* printf "%a@." TreeAutomaton.pp ta; *)
    let t  = load_tree Sys.argv.(2) in
    (* printf "%a@." Tree.pp t; *)
    let tmap = map_of_binary_tree t in
    let r_opt = TreeAutomaton.validate_opt ta tmap "" ta.initial in
    (* printf "%a@." pp_string_set r_opt *)
    if StringSet.mem "accept" r_opt then printf "Valide@."
    else printf "Invalide@."
  end
  else begin
    if Array.length Sys.argv > 1 then
      Array.iter (fun arg ->
          if arg = "--help" then begin printf "%a@." print_help (); exit 1 end
        ) Sys.argv;
    printf "Nombre incorrect d'arguments.\n\n%a@."
      print_help ();
    exit 1
  end
