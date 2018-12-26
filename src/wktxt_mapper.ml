open Wktxt_type

type wktxt_document_mapper =
{
  document : wktxt_document_mapper -> document -> document
  ; block  : wktxt_document_mapper -> block -> block
  ; table_block : wktxt_document_mapper -> table_block -> table_block
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
  | Table (title, content_list) ->
      Table ((List.map (self.inline self) title)
             , (List.map (fun l -> List.map (self.table_block self) l) content_list))
  | Hrule -> Hrule

and table_block self t_blck = match t_blck with
  | TableHead content -> TableHead (List.map (self.inline self) content)
  | TableItem content -> TableItem (List.map (self.inline self) content)

and inline self inl = match inl with
  | Italic l ->
      Italic (List.map (self.inline self) l)
  | Bold l ->
      Bold (List.map (self.inline self) l)
  | String str -> String str
  | Link str -> Link str
  | ExtLink str -> ExtLink str

let default_mapper =
  { document
  ; block
  ; table_block
  ; inline
}

let get_table_of_content doc =
  let toc_list = ref [] in
  let block self blck =
    match blck with
    | Header (id, depth, inlines) when depth <> 1 ->
      let link = (String ("<a href=\"#" ^ id ^ "\">") :: inlines) @ [String "</a>"] in
      toc_list := ((Ordered, depth - 1), [link]) :: !toc_list ;
      blck
    | _ -> default_mapper.block self blck
  in
  let mapper = { default_mapper with block } in
  let () = ignore (mapper.document mapper doc) in
  let toc_ast_list = Wktxt_parsing_functions.parse_list 0 (List.rev !toc_list) Ordered in
  match toc_ast_list with
  | [] -> NumList []
  | _ -> List.hd (List.flatten toc_ast_list)

let set_table_of_content doc =
  let toc_ast = get_table_of_content doc in
  let block self blck =
    match blck with
    | Paragraph [String "__TOC__" ; String "\n"] -> toc_ast
    | List _ | NumList _ | DefList _ -> blck
    | _ -> default_mapper.block self blck
  in
  let mapper = { default_mapper with block } in
  mapper.document mapper doc

let create_link link =
  let get_link url text =
    "<a href=\"" ^ url ^ "\">" ^ text ^ "</a>"
  in
  let length = String.length link in
  match String.index_opt link ' ' with
  | None -> get_link link link
  | Some space_pos ->
    get_link (String.sub link 0 space_pos) (String.sub link (space_pos + 1) (length - space_pos - 1))

let set_links doc =
  let inline self inl =
    match inl with
    | ExtLink s -> ExtLink (create_link s)
    | _ -> default_mapper.inline self inl
  in
  let mapper = { default_mapper with inline } in
  mapper.document mapper doc
