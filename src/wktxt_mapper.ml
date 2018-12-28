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

(** [get_table_of_content doc] returns a Numbered list (NumList) that contains every subtitle
    found in the given document. Each list item is a link to the homonym subtitle. *)
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

(** [set_table_of_content doc] if a Paragraph that contains "__TOC__" followed by a newline is found in doc,
    replaces it by the table of content.
    If it is not found, does nothing. *)
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

(** [create_link raw_link] returns the html equivalent of the given link that is "url some_text" *)
let create_link raw_link =
  let get_link url text =
    "<a href=\"" ^ url ^ "\">" ^ text ^ "</a>"
  in
  let link = String.trim raw_link in
  let length = String.length link in
  match String.index_opt link ' ' with
  | None -> get_link link link
  | Some space_pos ->
    get_link (String.sub link 0 space_pos) (String.sub link (space_pos + 1) (length - space_pos - 1))

(** [set_links doc] returns doc with every links converted in html *)
let set_links doc =
  let inline self inl =
    match inl with
    | ExtLink s -> ExtLink (create_link s)
    | _ -> default_mapper.inline self inl
  in
  let mapper = { default_mapper with inline } in
  mapper.document mapper doc

(** [normalize_blocks doc] Whenever it is possible, concatenates Strings together and removes any trailing spaces
    at the end of each block, then returns the document. *)
let normalize_blocks doc =
  let rec concat_inlines inlines =
    match inlines with
    | [] -> []
    | [ Bold inl ] -> [ Bold (concat_inlines inl) ]
    | [ Italic inl ] -> [ Italic (concat_inlines inl) ]
    | String s1 :: String s2 :: tl -> concat_inlines (String (s1 ^ s2) :: tl)
    | hd :: tl -> hd :: concat_inlines tl
  and concat_trim_inlines inlines =
    let rec remove_trailing_spaces str position =
    if position < 0 then ""
    else begin
      match str.[position] with
      | '\n' | ' ' | '\t' -> remove_trailing_spaces str (position - 1)
      | _ -> String.sub str 0 (position + 1)
      end
    in
    match inlines with
    | [] -> []
    | [ Bold inl ] -> [ Bold (concat_trim_inlines inl) ]
    | [ Italic inl ] -> [ Italic (concat_trim_inlines inl) ]
    | [ String s ] -> [String (remove_trailing_spaces s (String.length s - 1))]
    | String s1 :: String s2 :: tl -> concat_trim_inlines (String (s1 ^ s2) :: tl)
    | Bold inl :: tl -> Bold (concat_inlines inl) :: concat_trim_inlines tl
    | Italic inl :: tl -> Italic (concat_inlines inl) :: concat_trim_inlines tl
    | hd :: tl -> hd :: concat_trim_inlines tl
  in
  let block self blck = match blck with
    | Header (id, importance, content) ->
        Header (id, importance, concat_trim_inlines content)
    | Paragraph (content) ->
        Paragraph (concat_trim_inlines content)
    | DefList (content_list) ->
        DefList (List.map
          (fun (l1, l2) ->
            (concat_trim_inlines l1, List.map (self.block self) l2))
          content_list)
    | Table (title, content_list) ->
        Table (concat_trim_inlines title, (List.map (fun l -> List.map (self.table_block self) l) content_list))
    | _ -> default_mapper.block self blck
  and table_block _ t_blck = match t_blck with
    | TableHead content -> TableHead (concat_trim_inlines content)
    | TableItem content -> TableItem (concat_trim_inlines content)
  and inline self inl = match inl with
    | Italic l ->
        Italic (concat_trim_inlines l)
    | Bold l ->
        Bold (concat_trim_inlines l)
    | _ -> default_mapper.inline self inl
  in
  let mapper = { default_mapper with block ; table_block ; inline} in
  mapper.document mapper doc
