
let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let doc = Wktxt_parser.document Wktxt_lexer.main lexbuf in
    print_endline (Wktxt_type.show_document doc)
  with
  | _ ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let col = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    failwith
      (Printf.sprintf "Error line %d, col %d, token %s" line col tok)
