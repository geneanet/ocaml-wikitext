open Wktxt_type

type wktxt_document_mapper =
{
  document : wktxt_document_mapper -> document -> document
  ; block  : wktxt_document_mapper -> block -> block
  ; inline : wktxt_document_mapper -> inline -> inline
}

let document self = List.map (self.block self)

and block self blck : block = match blck with
  | Header (importance, content) ->
      Header (importance, (List.map (self.inline self) content))
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
