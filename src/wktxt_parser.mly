%{
  open Wktxt_type
  (* pair_list : ((bool, int), inlines) *)


  let rec get_pair_list_from_depth depth pair_list =
    match pair_list with
      | ((_,d), _) :: tl when d > depth -> get_pair_list_from_depth depth tl
      | list -> list

  let rec get_blocks depth pair_list = (* -> block list list *)
    match pair_list with
    | ((_,next_depth), next_paragraph) :: tl when depth = next_depth ->
      begin match tl with
        | ((_,d'), _) :: _ when next_depth < d' ->
          [Paragraph (List.flatten next_paragraph) ; List (get_blocks (depth + 1) tl)] ::
          get_blocks depth (get_pair_list_from_depth depth tl)
        | _ ->
          [Paragraph (List.flatten next_paragraph)] :: get_blocks depth tl
      end
    | ((_,next_depth), _) :: tl when depth < next_depth ->
      [List (get_blocks (depth + 1) pair_list )] :: get_blocks depth tl
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
      match List.hd l with
      | ((list_type, _), _) when list_type = Ordered ->
        NumList (get_blocks 1 l)
      | _ ->
        List (get_blocks 1 l)
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
