{
  open Parser
}

let letters = ['a'-'z' 'A'-'Z']
let digits  = ['0'-'9']
let number  = digits+
let whitespaces = [' ' '\t' '\n']

rule lexer = parse
  | whitespaces* '\n' whitespaces* { Lexing.new_line lexbuf; ETYPE }
  | whitespaces+ {
      (* tous les espaces blancs ne contenant pas de retour à la ligne *)
      lexer lexbuf
    }
  | '='    { EQUAL }
  | '*'    { STAR  }
  | '?'    { QMARK }
  | '~'    { TILDE }
  | '|'    { ALT   }
  | '['    { LSQ   }
  | ']'    { RSQ   }
  | '('    { LPAR  }
  | ')'    { RPAR  }
  | "type" { TYPE  }
  | letters (letters|digits|'_')* as s { IDENT s }
  | eof { EOF }
  | _ { failwith "lexical error" }
{ }
