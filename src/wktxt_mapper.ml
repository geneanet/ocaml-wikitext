(**
   {!mapper} allows to implement AST rewriting using open recursion.
   A typical mapper would be based on {!default_mapper}, a deep
   identity mapper, and will fall back on it for handling the syntax it
   does not modify.
*)

open Wktxt_type

type mapper =
  { document : mapper -> document -> document
  ; block  : mapper -> block -> block
  ; table_block : mapper -> table_block -> table_block
  ; inline : mapper -> inline -> inline
  }

(**/**)

let document self = List.map (self.block self)

and block self = function
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

and table_block self = function
  | TableHead content -> TableHead (List.map (self.inline self) content)
  | TableItem content -> TableItem (List.map (self.inline self) content)

and inline self = function
  | Italic l -> Italic (List.map (self.inline self) l)
  | Bold l -> Bold (List.map (self.inline self) l)
  | String str -> String str
  | Link str -> Link str
  | ExtLink str -> ExtLink str
(**/**)

(** A default mapper, which implements a "deep identity" mapping. *)
let default_mapper =
  { document
  ; block
  ; table_block
  ; inline
  }

(** [toc doc]
    Compute the table of contents of [doc]. This table of contents
    is computed by looking at headers. First level header is omitted.
*)
let toc
  : document -> block option =
  fun doc ->
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
  match Wktxt_parsing_functions.parse_list 0 (List.rev !toc_list) Ordered with
  | [] -> None
  | toc :: _ -> Some toc

(**
*)
let set_toc doc =
  match toc doc with
  | None -> doc
  | Some toc ->
    let block self blck =
      match blck with
      | Paragraph [String "__TOC__" ; String "\n"] -> toc
      | List _ | NumList _ | DefList _ -> blck
      | _ -> default_mapper.block self blck
    in
    let mapper = { default_mapper with block } in
    mapper.document mapper doc

(** [link sep str]
    A very basic link creation. No escaping is performed.
    Turn [str] into a link (["<a href=\"%s\">%s</a>"]).
    If [str] contains a [sep] character, everything coming before is
    used as the url, and the rest as text.
*)
let link : char -> string -> string =
  fun sep str ->
  let link url txt = Printf.sprintf "<a href=\"%s\">%s</a>" url txt in
  match String.index_opt str sep with
  | None -> link str str
  | Some space_pos ->
    link
      (String.sub str 0 space_pos)
      (String.sub str (space_pos + 1) (String.length str - space_pos - 1))

(**
   [set_links doc]
   Replace [Link] and [ExtLink] occurences by their HTML representation.
   [Link] uses ['|'] as separator and [ExtLink] uses [' '].
   using {!val:link}
*)
let set_links doc =
  let inline self inl =
    match inl with
    | ExtLink s -> String (link ' ' s)
    | Link s -> String (link '|' s)
    | _ -> default_mapper.inline self inl
  in
  let mapper = { default_mapper with inline } in
  mapper.document mapper doc
