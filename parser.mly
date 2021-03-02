%token STAR QMARK
%token ALT LPAR RPAR
%token LSQ RSQ EQUAL
%token <string> IDENT
%token TILDE TYPE ETYPE EOF

%start <Types.type_def list> main
%%

main:
  tlist = separated_list(ETYPE, type_def); EOF { tlist }
;

type_def:
| TYPE; type_id = IDENT; EQUAL; label = label; LSQ; RSQ {
    ({ id = type_id; label = label; regexp = Types.Empty } : Types.type_def)
  }
| TYPE; type_id = IDENT; EQUAL; label = label; LSQ; r = regexp_alt; RSQ {
    ({ id = type_id; label = label; regexp = r } : Types.type_def)
  }
;

label:
| id = IDENT                              { Types.Ident id }
| STAR                                    { Types.Any      }
(* inutile car finalement les exclusions d'étiquettes sont des chaînes de
 * caractères *)
(* | LPAR; l = label; RPAR                   { l } *)
| TILDE; l = separated_list(TILDE, IDENT) { Types.AllBut l }

regexp_base:
| id = IDENT                 { Types.Atom id         }
| r = regexp_base; STAR      { Types.Star r          }
| r = regexp_base; QMARK     { Types.QMark r         }
| LPAR; r = regexp_alt; RPAR { r }
;

regexp_concat:
| r = regexp_base                      { r }
| r1 = regexp_base; r2 = regexp_concat { Types.Concat(r1, r2)  }
;

regexp_alt:
| r = regexp_concat                        { r }
| r1 = regexp_concat; ALT; r2 = regexp_alt { Types.Alt(r1, r2)     }
;
