{
  open Wktxt_parser

  let debug = false
  let newline = ref true
  let last_def_term_line = ref 0
  let last_def_term_depth = ref 0
  let in_table = ref false

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

  let newline_token_or_str (str, token) lexbuf =
    Lexing.new_line lexbuf ;
    if !newline then
      token
    else begin
      newline := true ;
      STRING str
    end

  let table_or_str (str, token) =
    match token with
    | TABLE_START ->
      in_table := true ;
      token
    | TABLE_END when !in_table ->
      in_table := false;
      token
    | _ when !in_table ->
      token
    | _ -> STRING str

  let update_lex_new_line lexbuf ch =
    if ch = '\n' then Lexing.new_line lexbuf else ()

  let get_lex_line lexbuf =
    let pos = lexbuf.Lexing.lex_start_p in
    pos.Lexing.pos_lnum
}

let hrule = "----" ['-']*
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
  | (ws* table_title ws*) as s {
      if debug && !newline then Printf.printf "TABLE_TITLE\n" ;
      let tok = token_or_str (s, TABLE_TITLE) in table_or_str (s, tok)
    }
  | (ws* table_start ws*) as s '\n' {
      let tok = newline_token_or_str (s, TABLE_START) lexbuf in table_or_str (s, tok)
    }
  | (ws* table_end ws*) as s {
      if debug && !newline then Printf.printf "TABLE_END\n" ;
      let tok = token_or_str (s, TABLE_END) in table_or_str (s, tok)
    }
  | (ws* table_new_line ws*) as s '\n' {
      let tok = newline_token_or_str (s, TABLE_NEW_LINE) lexbuf in table_or_str (s, tok)
    }
  | (ws* cell_inline ws*) as s {
      let tok = str_or_token (s, TABLE_CELL TableCell) in table_or_str (s, tok)
    }
  | (ws* header_cell_inline ws*) as s {
      let tok = str_or_token (s, TABLE_CELL TableHeader) in table_or_str (s, tok)
    }
  | (ws* cell ws*) as s {
      if debug && !newline then Printf.printf "TABLE_CELL Cell\n" ;
      let tok = token_or_str (s, TABLE_CELL TableCell) in table_or_str (s, tok)
    }
  | (ws* header_cell ws*) as s {
      if debug && !newline then Printf.printf "TABLE_CELL Header\n" ;
      let tok = token_or_str (s, TABLE_CELL TableHeader) in table_or_str (s, tok)
    }
  | (ws* (':'* ';') as s1 ws*) as s2 {
      token_or_str (s2, DEFLIST (Term, String.length s1))
    }
  | (ws* ':'+ as s1 ws*) as s2 {
      token_or_str (s2, DEFLIST (Description, String.length s1))
    }
  | (ws* '='+ as s1 ws*) as s2 {
      if debug then Printf.printf "HEADER START %d\n" (String.length s1) ;
      token_or_str (s2, HEADER (String.length s1))
    }
  | (ws* '*'+ as s1 ws*) as s2 {
      if debug then Printf.printf "LIST %d\n" (String.length s1) ;
      token_or_str (s2, LIST (Unordered, String.length s1))
    }
  | (ws* '#'+ as s1 ws*) as s2 {
      if debug then Printf.printf "NUMLIST %d\n" (String.length s1);
      token_or_str (s2, LIST (Ordered, String.length s1))
    }
  | (ws* hrule ws*) as s {
      if debug then Printf.printf "HRULE\n" ;
      token_or_str (s, HRULE )
    }
  | ws* '='+ as s ws* '\n' {
      if debug then Printf.printf "HEADER END %d \n" (String.length s) ;
      Lexing.new_line lexbuf ;
      newline := true ;
      HEADER (String.length s)
    }
  | ws* '\n' as s {
     newline_token_or_str (s, EMPTYLINE) lexbuf
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
  | "'"+ as s {
    newline := false ;
    match String.length s with
    | 2 -> ITALIC
    | 3 -> BOLD
    | 5 -> BOLDITALIC
    | _ -> STRING s
  }
  | eof _* {
      if debug then Printf.printf "EOF\n" ;
      EOF
    }
