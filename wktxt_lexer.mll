{
  open Wktxt_parser

  let debug = true
  let newline = ref true

  (* Retourne soit [String str], soit [token], suivant si
    on est en début de ligne ou pas.
  *)
  let token_or_str (str, token) =
    if !newline then begin
      newline := false ;
      token
    end
    else begin
      STRING str
    end
}

let bold = "'''"
let italic = "''"
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']
let white = [ ' ' '\t']

rule main = parse
  | '='+ as s {
      token_or_str (s, HEADER (String.length s))
    }
  | '\n' {
      print_endline __LOC__ ;
      Lexing.new_line lexbuf ;
      newline := true ;
      main lexbuf
    }
  | (alphanum+ white?)+ as s { STRING s }
  | white+ { WHITE }
  | italic { ITALIC }
  | bold { BOLD }
  | eof { EOF }
  | _ as c { CHAR c }
