module Type = Wktxt_type
module Mapper = Wktxt_mapper

(** [ParsingError (line, column, lexeme)]  *)
exception ParsingError of int * int * string

(** [doc_from_lexbuf lex] parse [lex] and return the resulting {!type:Type.document}.
    Raise {!exception:ParsingError} in case of failure *)
let doc_from_lexbuf lexbuf =
  try
    Wktxt_lexer.newline := true ;
    Wktxt_lexer.last_def_term_line := 0 ;
    Wktxt_lexer.last_def_term_depth := 0 ;
    Wktxt_lexer.in_table := false ;
    Wktxt_lexer.header_isopen := false ;
    Wktxt_parser.document Wktxt_lexer.main lexbuf
  with _ ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let col = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let lexeme = Lexing.lexeme lexbuf in
    raise (ParsingError (line, col, lexeme))

(** See {!val:doc_from_lexbuf} *)
let doc_from_string string =
  doc_from_lexbuf (Lexing.from_string string)

(** See {!val:doc_from_lexbuf} *)
let doc_from_channel chan =
  doc_from_lexbuf (Lexing.from_channel chan)

(** See {!val:doc_from_lexbuf} *)
let doc_from_file file =
  let chan = open_in file in
  let doc = doc_from_channel chan in
  close_in chan ;
  doc

(** [output_document out doc] runs through the parse tree doc
    and prints its content translated into HTML code using the out function *)
let output_document =
  Wktxt_output.output_document

(** See {!val:output_document} *)
let doc_to_string doc =
  let buffer = Buffer.create 4096 in
  let () = output_document (Buffer.add_string buffer) doc in
  Buffer.contents buffer

(** See {!val:output_document} *)
let doc_to_chan doc chan =
  let str_doc = doc_to_string doc in
  output_string chan str_doc

(** See {!val:output_document} *)
let doc_to_file doc filename =
  let chan = open_out filename in
  let () = doc_to_chan doc chan in
  close_out chan
