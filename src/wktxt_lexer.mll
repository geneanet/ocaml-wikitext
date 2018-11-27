{
  open Wktxt_parser

  let debug = false
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
let ws = [ ' ' '\t']
let wordchar = [^''' '=' '*' '#' '\n' '[' ']']
let linkchar = [^'[' ']']

rule main = parse
  | '='+ as s ws* {
      if debug then Printf.printf "HEADER START %d\n" (String.length s) ;
      token_or_str (s, HEADER (String.length s))
    }
  | '*'+ as s ws*{ 
      if debug then Printf.printf "LIST %d\n" (String.length s) ;
      token_or_str (s, LIST (String.length s))
    }
  | '#'+ as s ws*{ 
      if debug then Printf.printf "NUMLIST %d\n" (String.length s);
      token_or_str (s, NUMLIST (String.length s))
    }
  | hrule as s ws*{
      if debug then Printf.printf "HRULE\n" ;
      token_or_str (s, HRULE )
    }
  | '='+ as s '\n' {
      if debug then Printf.printf "HEADER END %d \n" (String.length s) ;
      Lexing.new_line lexbuf ;
      newline := true ;
      HEADER (String.length s)
    }
  | ws*'\n' {
      Lexing.new_line lexbuf ;
      if !newline then begin
        if debug then Printf.printf "EMPTYLINE\n" ;
        EMPTYLINE
      end
      else begin
        newline := true ;
        STRING "\n"
      end
    }
  | "[[" (linkchar+ as s) "]]" {
      if debug then Printf.printf "LINK : %s\n" s ;
      LINK s
    }
  | "[" (linkchar+ as s) "]" {
      if debug then Printf.printf "EXTLINK : %s\n" s ;
      EXTLINK s
    }
  | wordchar+ as s {
      newline := false ;
      if debug then Printf.printf "STRING : %s\n" s ;
      STRING s
    }
  | italic {
      if debug then Printf.printf "ITALIC\n" ;
      ITALIC 
    }
  | bold {
      if debug then Printf.printf "BOLD\n" ;
      BOLD
    }
  | bold italic {
      if debug then Printf.printf "BOLDITALIC\n" ;
      BOLDITALIC
    }
  | eof _* {
      if debug then Printf.printf "EOF\n" ;
      EOF
    }
