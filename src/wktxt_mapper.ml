open Wktxt_type

type wktxt_document_mapper =
{
  document : wktxt_document_mapper -> document -> document
  ; block  : wktxt_document_mapper -> block -> block
  ; inline : wktxt_document_mapper -> inline -> inline
}

let document self = List.map (self.block self)

and block self blck : block = match blck with
  | Header (id, importance, content) ->
      Header (id, importance, (List.map (self.inline self) content))
  | Paragraph (content) ->
      Paragraph (List.map (self.inline self) content)
  | List (content_list) ->
      List (List.map (fun l -> List.map (self.block self) l) content_list)
  | NumList (content_list) ->
      NumList (List.map (fun l -> List.map (self.block self) l) content_list)
  | DefList (content_list) ->
      DefList (List.map
        (fun (l1, l2) ->
          (List.map (self.inline self) l1, List.map (self.block self) l2))
        content_list)
  | _ -> blck

and inline self inl = match inl with
  | Italic l ->
      Italic (List.map (self.inline self) l)
  | Bold l ->
      Bold (List.map (self.inline self) l)
  | _ -> inl

let default_mapper =
  { document
  ; block
  ; inline
}

let set_table_of_content doc =
  let open Wktxt_type in
  let toc_list = ref [] in
  let block self blck =
    match blck with
    | Header (_, depth, inlines) when depth <> 1 ->
      toc_list := ((Ordered, depth - 1), [inlines]) :: !toc_list ;
      blck
    | _ -> default_mapper.block self blck
  in
  let mapper = { default_mapper with block } in
  let () = ignore (mapper.document mapper doc) in
  let toc_ast = List.hd (List.flatten (Wktxt_parsing_functions.parse_list 0 (List.rev !toc_list) Ordered)) in
  let block self blck =
    match blck with
    | Paragraph [String "__TOC__" ; String "\n"] -> toc_ast
    | List _ | NumList _ | DefList _ -> blck
    | _ -> default_mapper.block self blck
  in
  let mapper = { default_mapper with block } in
  mapper.document mapper doc
