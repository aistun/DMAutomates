open Format
open Types
open Xml

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

type binary_tree_map = string StringMap.t

let rec parse_xml xml next =
  match xml, next with
  | PCData _, _ -> assert false
  | Element(label, _, []), [] ->
    Node(label, Empty, Empty)
  | Element(label, _, []), x :: next ->
    Node(label, Empty, parse_xml x next)
  | Element(label, _, c :: children), [] ->
    Node(label, parse_xml c children, Empty)
  | Element(label, _, c :: children), x :: next ->
    Node(label, parse_xml c children, parse_xml x next)

let map_of_binary_tree t =
  let rec aux map p = function
    | Empty -> StringMap.add p "#" map
    | Node(label, l, r) ->
      let map' = StringMap.add p label map in
      let mapl = aux map' (p ^ "1") l in
      aux mapl (p ^ "2") r in
  aux StringMap.empty "" t

let load_tree file_name =
  let doc = Xml.parse_file file_name in
  parse_xml doc []

let rec pp f t =
  match t with
  | Empty -> fprintf f "#"
  | Node(label, l, r) ->
    fprintf f "%s" label;
    pp_open_box f (0);
    fprintf f " - %a" pp l;
    pp_force_newline f ();
    fprintf f " \\";
    pp_force_newline f ();
    fprintf f "   %a" pp r;
    pp_close_box f ()
