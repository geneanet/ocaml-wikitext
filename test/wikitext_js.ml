open Js_of_ocaml

let () =
  let getElementById coerce id =
    match Js.Opt.to_option @@ Dom_html.document##getElementById (Js.string id) with
    | None -> failwith id
    | Some x -> match Js.Opt.to_option @@ coerce x with
                | None -> failwith id
                | Some x -> x
  in
  let input = getElementById Dom_html.CoerceTo.textarea "input" in
  let output = getElementById Dom_html.CoerceTo.div "output" in
  let update () =
    let txt = Js.to_string input##.value in
    let lexbuf = Lexing.from_string txt in
    try
      let doc = Wktxt_parser.document Wktxt_lexer.main lexbuf in
      output##.innerHTML := Js.string @@ Wktxt_type.show_document doc
    with
    | _ ->
       let curr = lexbuf.Lexing.lex_curr_p in
       let line = curr.Lexing.pos_lnum in
       let col = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
       let tok = Lexing.lexeme lexbuf in
       output##.innerHTML :=
         Js.string @@
           Printf.sprintf "Error line %d, col %d, token [%s]" line col tok
  in
  input##.oninput := Dom.handler (fun _ -> update () ; Js._false) ;
  update ()
