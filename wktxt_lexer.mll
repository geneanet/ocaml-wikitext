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

let hrule = "----"
let bold = "'''"
let italic = "''"
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']
let wordchar = (alphanum) | ['.' ',' '-' '"']
let ws = [ ' ' '\t']

rule main = parse
  | '='+ as s {
      token_or_str (s, HEADER (String.length s))
    }
  | '*'+ as s { 
      token_or_str (s, LIST (String.length s))
    }
  | '#'+ as s { 
      token_or_str (s, NUMLIST (String.length s))
    }
  | hrule as s {
      token_or_str (s, HRULE )
    }
  | ws*'\n' {
      Lexing.new_line lexbuf ;
      if !newline then
        EMPTYLINE
      else begin
        newline := true ;
        main lexbuf
      end
    }
  | '='+ as s '\n' {
      Lexing.new_line lexbuf ;
      newline := true ;
      HEADER (String.length s)
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
