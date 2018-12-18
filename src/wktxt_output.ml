open Wktxt_type

let rec output_document out doc :(unit)= List.iter (output_block out) doc

and display_item displayer tag_name out content :(unit)=
  begin
  out ("<" ^ tag_name ^ ">") ;
  List.iter (displayer out) content ;
  out ("<\\" ^ tag_name ^ ">")
  end

and output_inline out inl :(unit)=
  match inl with
  | Bold (content) ->
    display_item output_inline "b" out content
  | Italic (content) ->
    display_item output_inline "i" out content
  | String str | Link str | ExtLink str -> out str

and output_block out blck :(unit)= 
  match blck with
  | Header (importance, content) ->
    display_item output_inline ("h" ^ (string_of_int importance)) out content
  | Paragraph (content) ->
    display_item output_inline "p" out content
  | List (content_list) ->
    display_item (display_item output_block "li" out) "ul" out content_list
  | NumList (content_list) ->
    display_item (display_item output_block "li" out) "ol" out content_list
  | DefList (content_list) ->
    let output_def_list_item item = match item with
      | ([],[]) -> ()
      | (term, []) -> display_item output_inline "dt" out term
      | ([], desc) -> List.iter (display_item output_inline "dd" out) desc
      | (term, desc) -> 
        display_item output_inline "dt" out term ;
        List.iter (display_item output_inline "dd" out) desc
    in
    display_item output_def_list_item "dl" out content_list
  | _ -> ()
