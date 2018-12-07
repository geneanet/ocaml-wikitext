{
  open Wktxt_parser

  let debug = false
  let newline = ref true
  let last_def_term_line = ref 0
  let last_def_term_depth = ref 0

  (* Retourne soit [String str], soit [token], suivant si
    on est en début de ligne ou pas.
  *)
  let token_or_str (str, token) =
    if !newline then begin
      newline := false ;
      token
    end
    else
      STRING str

  let str_or_token (str, token) =
    if !newline then begin
      newline := false ;
      STRING str
    end
    else
      token

  let newline_token_or_str str token lexbuf =
    Lexing.new_line lexbuf ;
    if !newline then
      token
    else begin
      newline := true ;
      STRING str
    end

  let update_lex_new_line lexbuf ch =
    if ch = '\n' then Lexing.new_line lexbuf else ()

  let get_lex_line lexbuf =
    let pos = lexbuf.Lexing.lex_start_p in
    pos.Lexing.pos_lnum
}

let hrule = "----" ['-']*
let bold = "'''"
let italic = "''"
let ws = [ ' ' '\t']
let wordchar = [^''' '=' '*' '#' '\n' '[' ']' ':' ';' '|' '!' '{' '}']
let linkchar = [^'[' ']']

let table_start = "{|"
let table_end = "|}"
let table_title = "|+"
let table_new_line = "|-" ['-']*
let cell_inline = "||"
let header_cell_inline = "!!"
let cell = "|"
let header_cell = "!"

(*
  When editing these lexing rules, do not forget to use and update the newline reference.
*)
rule main = parse
  | ws* table_title as s ws* {
      if debug && !newline then Printf.printf "TABLE_TITLE\n" ;
      token_or_str (s, TABLE_TITLE)
    }
  | ws* table_start as s ws* '\n' {
     newline_token_or_str s TABLE_START lexbuf
    }
  | ws* table_end as s ws* {
      if debug && !newline then Printf.printf "TABLE_END\n" ;
      token_or_str (s, TABLE_END)
    }
  | ws* table_new_line as s ws* '\n' {
     newline_token_or_str s TABLE_NEW_LINE lexbuf
    }
  | ws* cell_inline as s ws* {
      str_or_token (s, TABLE_CELL TableCell)
    }
  | ws* header_cell_inline as s ws* {
      str_or_token (s, TABLE_CELL TableHeader)
    }
  | ws* cell as s ws* {
      if debug && !newline then Printf.printf "TABLE_CELL Cell\n" ;
      token_or_str (s, TABLE_CELL(TableCell))
    }
  | ws* header_cell as s ws* {
      if debug && !newline then Printf.printf "TABLE_CELL Header\n" ;
      token_or_str (s, TABLE_CELL(TableHeader))
    }
  | ws* (':'* ';') as s ws* {
      token_or_str (s, DEFLIST(Term, String.length s))
    }
  | ws* ':'+ as s ws* {
      token_or_str (s, DEFLIST(Description, String.length s))
    }
  | ws* '='+ as s ws* {
      if debug then Printf.printf "HEADER START %d\n" (String.length s) ;
      token_or_str (s, HEADER (String.length s))
    }
  | ws* '*'+ as s ws*{ 
      if debug then Printf.printf "LIST %d\n" (String.length s) ;
      token_or_str (s, LIST (Unordered, String.length s))
    }
  | ws* '#'+ as s ws*{ 
      if debug then Printf.printf "NUMLIST %d\n" (String.length s);
      token_or_str (s, LIST (Ordered, String.length s))
    }
  | ws* hrule as s ws*{
      if debug then Printf.printf "HRULE\n" ;
      token_or_str (s, HRULE )
    }
  | ws* '='+ as s '\n' {
      if debug then Printf.printf "HEADER END %d \n" (String.length s) ;
      Lexing.new_line lexbuf ;
      newline := true ;
      HEADER (String.length s)
    }
  | ws* '\n' as s {
     newline_token_or_str s EMPTYLINE lexbuf
    }
  | "[[" (linkchar+ as s) "]]" {
      newline := false;
      if debug then Printf.printf "LINK : %s\n" s ;
      String.iter (update_lex_new_line lexbuf) s;
      LINK s
    }
  | "[" (linkchar+ as s) "]" {
      newline := false;
      if debug then Printf.printf "EXTLINK : %s\n" s ;
      String.iter (update_lex_new_line lexbuf) s;
      EXTLINK s
    }
  | wordchar+ as s {
      newline := false ;
      if debug then Printf.printf "STRING : %s\n" s ;
      STRING s
    }
  | italic {
      newline := false;
      if debug then Printf.printf "ITALIC\n" ;
      ITALIC 
    }
  | bold {
      newline := false;
      if debug then Printf.printf "BOLD\n" ;
      BOLD
    }
  | bold italic {
      newline := false;
      if debug then Printf.printf "BOLDITALIC\n" ;
      BOLDITALIC
    }
  | eof _* {
      if debug then Printf.printf "EOF\n" ;
      EOF
    }
