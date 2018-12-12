let set_table_of_content doc =
  let open Wktxt_mapper in
  let open Wktxt_type in
  let toc_list = ref [] in
  let block self blck =
    match blck with
    | Header (depth, inlines) -> Printf.printf "Header found : depth = %d\n" depth ;
      toc_list := ((Ordered, depth), inlines) :: !toc_list ;
      blck
    | _ -> default_mapper.block self blck in
  let mapper = { default_mapper with block } in
  match mapper.document mapper doc with _ -> () in
  let toc_ast = Wktxt_parser.parse_list (List.ref !toc_list) in
  let block self blck =
    match blck with
    (* TODO handle multi-string paragraphs *)
    | Paragraph l when l = [String "__TOC__"] -> toc_ast
    | List | NumList | DefList | _ -> blck in
  let mapper = { default_mapper with block } in
  mapper.document mapper doc

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let doc = Wktxt_parser.document Wktxt_lexer.main lexbuf in
    print_endline (Wktxt_type.show_document doc) ;
    match set_table_of_content doc with
    | _ -> ()
  with
  | _ ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let col = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    failwith
      (Printf.sprintf "Error line %d, col %d, token [%s]" line col tok)
