%{
  open Wktxt_type
  (* pair_list : ((bool, int), inlines) *)

  let rec get_pair_list_from_depth depth pair_list =
    match pair_list with
      | ((_, d), _) :: tl when d > depth -> get_pair_list_from_depth depth tl
      | list -> list

  let rec get_blocks depth pair_list prev_type = (* -> block list list *)
    match pair_list with
    | ((cur_type, next_depth), inlines) :: tl when depth = next_depth ->
      if cur_type <> prev_type then
        prerr_string "Warning : Two list items of different type have been declared on the same level.\n"
        ;
      begin match tl with
        | ((next_type, d'), _) :: _ when next_depth < d' && next_type = Unordered ->
            [Paragraph (List.flatten inlines) ; List (get_blocks (depth + 1) tl Unordered )] ::
              get_blocks depth (get_pair_list_from_depth depth tl) prev_type
        | ((next_type, d'), _) :: _ when next_depth < d' && next_type = Ordered ->
            [Paragraph (List.flatten inlines) ; NumList (get_blocks (depth + 1) tl Ordered )] ::
              get_blocks depth (get_pair_list_from_depth depth tl) prev_type
        | _ ->
          [Paragraph (List.flatten inlines)] :: get_blocks depth tl prev_type
      end
    | ((next_type, next_depth), _) :: tl when depth < next_depth && next_type = Unordered ->
      [List (get_blocks (depth + 1) pair_list Unordered)] :: get_blocks depth tl prev_type
    | ((next_type, next_depth), _) :: tl when depth < next_depth && next_type = Ordered ->
      [NumList (get_blocks (depth + 1) pair_list Ordered)] :: get_blocks depth tl prev_type
    | _ -> []
%}

%token<int> HEADER
%token<Wktxt_type.order*int> LIST
%token<string> STRING LINK EXTLINK
%token ITALIC BOLD BOLDITALIC
%token EOF HRULE EMPTYLINE

%start document
%type <Wktxt_type.document> document

%%

document:
  | b = block* EOF { b }
;

block:
  | h1 = HEADER i = inline(regular)+ HEADER EMPTYLINE* { 
      Header (h1, (List.flatten i))
    }
  | l = pair(LIST, inline(regular)+)+ EMPTYLINE* {
      match List.hd l with (* TODO use another function before get_blocks to handle singles*)
      | ((list_type, _), _) when list_type = Ordered ->
        NumList (get_blocks 1 l Ordered)
      | _ ->
        List (get_blocks 1 l Unordered)
    }
  | HRULE EMPTYLINE* { Hrule }
  | i = inline(regular)+ EMPTYLINE* { Paragraph (List.flatten i) }
;

(* inlines *)

regular:
  | ITALIC i = inline(noformat)+ ITALIC { [Italic (List.flatten i)] }
  | BOLD i = inline(noformat)+ BOLD { [Bold (List.flatten i)] }
  | BOLDITALIC i = inline(noformat)+ BOLDITALIC {
      [Bold [ Italic (List.flatten i) ]]
    }
  | BOLDITALIC i1 = inline(noformat)+ ITALIC i2 = inline(noformat)+ BOLD {
      [Bold (Italic (List.flatten i1) :: (List.flatten i2))]
    }
  | BOLDITALIC i1 = inline(noformat)+ BOLD i2 = inline(noformat)+ ITALIC {
      [Italic (Bold (List.flatten i1) :: (List.flatten i2))]
    }
  | ITALIC i1 = inline(noformat)+ BOLD i2 = inline(noformat)+ BOLDITALIC {
      [Italic ( (List.flatten i1) @ [Bold (List.flatten i2)] )]
    }
  | BOLD i1 = inline(noformat)+ ITALIC i2 = inline(noformat)+ BOLDITALIC {
      [Bold ( (List.flatten i1) @ [Italic (List.flatten i2)] )]
    }
  | BOLD i1 = inline(noformat)+ ITALIC i2 = inline(noformat)+ ITALIC i3 = inline(noformat)+ BOLD {
      [Bold (List.flatten i1 @ [Italic (List.flatten i2)] @ List.flatten i3)]
    }
  | ITALIC i1 = inline(noformat)+ BOLD i2 = inline(noformat)+ BOLD i3 = inline(noformat)+ ITALIC {
      [Italic (List.flatten i1 @ [Bold (List.flatten i2)] @ List.flatten i3)]
    }
  | BOLD i1 = inline(noformat)+ BOLDITALIC i2 = inline(noformat)+ ITALIC {
      [Bold (List.flatten i1) ; Italic (List.flatten i2)]
    }
  | ITALIC i1 = inline(noformat)+ BOLDITALIC i2 = inline(noformat)+ BOLD {
      [Italic (List.flatten i1) ; Bold (List.flatten i2)]
    }
;

noformat:
  | s = STRING { [String s] }
  | s = LINK { [Link s] }
  | s = EXTLINK { [ExtLink s] }
;

inline(param):
  | s = STRING { [String s] }
  | s = LINK { [Link s] }
  | s = EXTLINK { [ExtLink s] }
  | p = param { p }
;

%%
