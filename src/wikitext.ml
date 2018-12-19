module Mapper = Wktxt_mapper

let get_current_parser_position lexbuf =
  let curr = lexbuf.Lexing.lex_curr_p in
  let line = curr.Lexing.pos_lnum in
  let col = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
  let lexeme = Lexing.lexeme lexbuf in
  Printf.sprintf "line %d, col %d, lexeme [%s]" line col lexeme

let doc_from_lexbuf lexbuf =
  Wktxt_lexer.newline := true ;
  Wktxt_lexer.last_def_term_line := 0 ;
  Wktxt_lexer.last_def_term_depth := 0 ;
  Wktxt_lexer.in_table := false ;
  Wktxt_parser.document Wktxt_lexer.main lexbuf

let output_document =
  Wktxt_output.output_document

let doc_to_string doc =
  let buffer = Buffer.create 4096 in
  let () = output_document (Buffer.add_string buffer) doc in
  Buffer.contents buffer

let doc_to_chan doc chan =
  let str_doc = doc_to_string doc in
  output_string chan str_doc

let doc_to_file doc filename =
  let chan = open_out filename in
  let () = doc_to_chan doc chan in
  close_out chan
