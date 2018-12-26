open Wikitext
let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let doc = doc_from_lexbuf lexbuf |> Mapper.set_table_of_content |> Mapper.set_links in
    output_document (Printf.printf "%s") doc
  with
  | _ ->
    failwith
      ("Error : " ^ get_current_parser_position lexbuf)
