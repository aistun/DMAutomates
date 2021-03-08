open Float
open Mlpost
open Picture
open Point
open Path
open Num
open Command
open Box
open Tree

let cross (x, y) r =
  let line1 = [x +. r *. cos (3. *. pi /. 4.), y +. r *. sin (3. *. pi /. 4.);
               x +. r *. cos (-.pi /. 4.), y +. r *. sin (-.pi /. 4.)] in
  let line2 = [x +. r *. cos (pi /. 4.), y +. r *. sin (pi /. 4.);
               x +. r *. cos (-.3. *. pi /. 4.), y +. r *. sin (-.3. *. pi /. 4.)] in
  [Path.draw (Path.path ~style:jLine ~scale:Num.cm line1);
   Path.draw (Path.path ~style:jLine ~scale:Num.cm line2)]

let state = Box.tex
    ~dx:(bp 4.)
    ~dy:(bp 4.)
    ~style:Circle
    ~stroke:(Some Color.black)

let rect_state = Box.tex
    ~dx:(bp 4.)
    ~dy:(bp 4.)
    ~style:Rect
    ~stroke:(Some Color.black)

let empty_rect_state ~name =
  Box.pic ~name:name ~dx:(bp 0.) ~dy:(bp 4.)
    (Path.draw ~color:Color.black
       (Path.scale (Num.pt 16.) Path.unitsquare))
let empty_state ~name =
  Box.pic ~name:name ~dx:(bp 0.) ~dy:(bp 4.)
    (Path.draw ~color:Color.white
       (Path.scale (Num.pt 16.) Path.unitsquare))


let final = Box.box ~style:Circle

let transition states tex anchor ?outd ?ind ?(pos=0.5)
    x_name y_name =
  let x = Box.get x_name states and y = Box.get y_name states in
  let outd = match outd with None -> None | Some a -> Some (vec (dir a)) in
  let ind = match ind with None -> None | Some a -> Some (vec (dir a)) in
  Arrow.draw ~tex ~anchor ~pos (cpath ?outd ?ind x y)

let loop states tex ?(pos=0.5) name =
  let box = Box.get name states in
  let a = Point.shift (Box.south box) (Point.pt (cm 0., cm (-0.4))) in
  let c = Box.ctr box in
  let p = Path.pathk [
    knotp ~r: (vec (dir 225.)) c;
    knotp a;
    knotp ~l: (vec (dir 135.)) c;
  ] in
  let bp = Box.bpath box in
  Arrow.draw ~tex ~pos ~anchor:`Bot (cut_after bp (cut_before bp p))

let loop_right states tex ?(pos=0.5) name =
  let box = Box.get name states in
  let a = Point.shift (Box.east box) (Point.pt (cm 0.4, cm 0.)) in
  let c = Box.ctr box in
  let p = Path.pathk [
    knotp ~r: (vec (dir 330.)) c;
    knotp a;
    knotp ~l: (vec (dir 210.)) c;
  ] in
  let bp = Box.bpath box in
  Arrow.draw ~tex ~pos ~anchor:`East (cut_after bp (cut_before bp p))


let initial ?(side=Box.west) ?(decal=Point.pt (cm (-0.3), zero))
    ?(anchor=`Top) ?(pos=0.) states name tex =
  let b = Box.get name states in
  let p = side b in
  Arrow.draw ~tex ~anchor ~pos (Path.pathp [ Point.shift p decal; p ])

let fnsize s = "\\footnotesize " ^ s

let clr c s = "{\\color{" ^ c ^ "}" ^ s ^ "}"
let tt s = "\\texttt{" ^ s ^ "}"
let ctt c s = "\\texttt{\\color{" ^ c ^ "}" ^ s ^ "}"
let mctt f l =
  "\\begin{tabular}{" ^ f ^ "}" ^
  List.fold_right (fun (c, x) acc ->
      if c = "" then tt (fnsize x) ^ "\\\\" ^ acc
      else ctt c (fnsize x) ^ "\\\\" ^ acc
    ) l ""
  ^ "\\end{tabular}"

let white_space4 = "\\:\\:\\:\\:"

let bdot p =
  Path.fill ~color:(Color.black)
    (Path.shift (cmp p) (Path.scale (Num.cm 0.3) fullcircle))

let line p1 p2 =
  Path.draw (Path.path ~style:jLine ~scale:Num.cm [p1; p2])

let text p t =
  (Picture.shift (cmp p)
     (Picture.scale (Num.cm 0.04) (Picture.tex t)))

let line_arrow ?(kind=Arrow.classic) p1 p2 =
  Arrow.draw ~kind (Path.path ~style:jLine ~scale:Num.cm [p1; p2])

let bdot_box = Box.pic
    (Path.fill ~color:(Color.black) (Path.scale (Num.cm 0.3) fullcircle))

let bdot_node ~color ~name =
  Box.pic ~name
    (Path.fill ~color (Path.scale (Num.cm 0.3) fullcircle))

let link states ?outd ?ind ?(color=Color.black) x_name y_name =
  let x = Box.get x_name states and y = Box.get y_name states in
  let outd = match outd with None -> None | Some a -> Some (vec (dir a)) in
  let ind = match ind with None -> None | Some a -> Some (vec (dir a)) in
  Path.draw ~color (* (Path.path ~style:jLine ~scale:Num.cm [x; p2]) *)
    (cpath ?outd ?ind x y)

let arrow states ?(color=Color.black) ?(dashed=None) ?outd ?ind
    x_name y_name =
  let x = Box.get x_name states and y = Box.get y_name states in
  let outd = match outd with None -> None | Some a -> Some (vec (dir a)) in
  let ind = match ind with None -> None | Some a -> Some (vec (dir a)) in
  let base_head = Arrow.head_classic ~color in
  let kind = match dashed with
    | None -> Arrow.add_head ~head:base_head (Arrow.add_line ~color Arrow.empty)
    | Some d -> Arrow.add_head ~head:(base_head ~dashed:d)
                  (Arrow.add_line ~color ~dashed:d Arrow.empty) in
  Arrow.draw ~kind (cpath ?outd ?ind x y)

let fig1 =
  let tex = Box.tex in
  let emp = tex " " in
  let nodes = Box.vbox ~padding:(cm 1.0) [
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"L1" "L1"
      ];
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"F1" "F1";
        tex ~name:"L2" "L2";
      ];
      Box.hbox ~padding:(cm 1.0) [
        emp; emp;
        tex ~name:"F2" "F2";
        tex ~name:"L3" "L3";
        tex ~name:"F5" "F5";
      ];
      Box.hbox ~padding:(cm 1.0) [
        emp; emp;
        tex ~name:"F3" "F3";
        tex ~name:"F4" "F4";
      ]
    ] in
  seq [
    Box.draw nodes;
    link nodes "L1" "F1";
    link nodes "L1" "L2";
    link nodes "L2" "F2";
    link nodes "L2" "L3";
    link nodes "L2" "F5";
    link nodes "L3" "F3";
    link nodes "L3" "F4";
  ]

let fig2 =
  let tex = Box.tex in
  let emp = tex " " in
  let rl = link ~color:(Color.red) in
  let nodes = Box.vbox ~padding:(cm 1.0) [
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"L1" "L1";
        tex ~name:"#1" "\\#";
      ];
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"F1" "F1";
        emp;emp;emp;emp;
        tex ~name:"L2" "L2";
        tex ~name:"#2" "\\#";
      ];
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"#3" "\\#";
        tex ~name:"F2" "F2";
        tex ~name:"L3" "L3";
        emp; emp;
        tex ~name:"F5" "F5";
        tex ~name:"#4" "\\#";
      ];
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"#5" "\\#";
        tex ~name:"F3" "F3";
        tex ~name:"F4" "F4";
        tex ~name:"#6" "\\#";
        tex ~name:"#7" "\\#";
      ];
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"#8" "\\#";
        tex ~name:"#9" "\\#";
        emp;
      ]
    ] in
  seq [
    Box.draw nodes;
    link nodes "L1" "F1";
    link nodes "L1" "L2";
    link nodes "L2" "F2";
    link nodes "L2" "L3";
    link nodes "L2" "F5";
    link nodes "L3" "F3";
    link nodes "L3" "F4";
    rl nodes "L1" "F1";
    rl nodes "F1" "L2";
    rl nodes "L2" "F2";
    rl nodes "F2" "L3";
    rl nodes "L3" "F5";
    rl nodes "L3" "F3";
    rl nodes "F3" "F4";
    rl nodes "L1" "#1";
    rl nodes "F1" "#3";
    rl nodes "L2" "#2";
    rl nodes "F5" "#4";
    rl nodes "F2" "#5";
    rl nodes "F4" "#6";
    rl nodes "F5" "#7";
    rl nodes "F3" "#8";
    rl nodes "F4" "#9";
  ]

let fig3 =
  let tex = Box.tex in
  let emp = tex " " in
  let rl = link ~color:(Color.red) in
  let nodes = Box.vbox ~padding:(cm 1.0) [
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"L1" "L1";
      ];
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"F1" "F1";
        tex ~name:"#1" "\\#";
      ];
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"#3" "\\#";
        tex ~name:"L2" "L2";
        emp;
      ];
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"F2" "F2";
        tex ~name:"#2" "\\#";
      ];
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"#5" "\\#";
        tex ~name:"L3" "L3";
        emp;
      ];
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"F3" "F3";
        tex ~name:"F5" "F5";
      ];
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"#8" "\\#";
        tex ~name:"F4" "F4";
        tex ~name:"#4" "\\#";
        tex ~name:"#7" "\\#";
      ];
      Box.hbox ~padding:(cm 1.0) [
        tex ~name:"#6" "\\#";
        tex ~name:"#9" "\\#";
        emp;
      ]
    ] in
  seq [
    Box.draw nodes;
    rl nodes "L1" "F1";
    rl nodes "F1" "L2";
    rl nodes "L2" "F2";
    rl nodes "F2" "L3";
    rl nodes "L3" "F5";
    rl nodes "L3" "F3";
    rl nodes "F3" "F4";
    rl nodes "L1" "#1";
    rl nodes "F1" "#3";
    rl nodes "L2" "#2";
    rl nodes "F5" "#4";
    rl nodes "F2" "#5";
    rl nodes "F4" "#6";
    rl nodes "F5" "#7";
    rl nodes "F3" "#8";
    rl nodes "F4" "#9";
  ]

let figs = [fig1; fig2; fig3]
let () =
  List.iteri (fun i x ->
      Metapost.emit ("figure-" ^ string_of_int (i + 1)) x
    ) figs
