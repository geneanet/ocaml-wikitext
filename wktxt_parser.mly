%{
  open Wktxt_type
%}

%token<int> HEADER LIST NUMLIST
%token<string> STRING
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
      Header (h1, i)
    }
  | l = LIST i = inline(regular)+ EMPTYLINE* {
      List (l, i)
    }
  | l = NUMLIST i = inline(regular)+ EMPTYLINE* {
      Num_list (l, i)
    }
  | HRULE EMPTYLINE* { Hrule }
  | i = inline(regular)+ EMPTYLINE* { Paragraph i }
;

(* inlines *)

regular:
  | ITALIC i = inline(italic)+ ITALIC { Italic i }
  | BOLD i = inline(bold)+ BOLD { Bold i }
  | BOLDITALIC i = inline(bolditalic)+ BOLDITALIC {
      Bold [ Italic i ]
    }
  | BOLDITALIC i1 = inline(bolditalic)+ ITALIC i2 = inline(bold)+ BOLD {
      Bold (Italic i1 :: i2)
    }
  | BOLDITALIC i1 = inline(bolditalic)+ BOLD i2 = inline(italic)+ ITALIC {
      Italic (Bold i1 :: i2)
    }
  | ITALIC i1 = inline(italic)+ BOLD i2 = inline(bolditalic)+ BOLDITALIC {
      Italic ( i1 @ [Bold i2] )
    }
  | BOLD i1 = inline(bold)+ ITALIC i2 = inline(bolditalic)+ BOLDITALIC {
      Bold ( i1 @ [Italic i2] )
    }

(*  Nouveau type pour envelopper les résultats de ceux deux là ?

  | BOLD i1 = inline(bold)+ BOLDITALIC i2 = inline(italic)+ ITALIC {
      Bold i1 @ Italic i2
    }
  | ITALIC i1 = inline(italic)+ BOLDITALIC i2 = inline(bold)+ BOLD {
      Italic i1 @ Bold i2
    }
*)
;

italic:
  | BOLD i = inline(bold)+ BOLD { Bold i }
;

bold:
  | ITALIC i = inline(italic)+ ITALIC { Italic i }
;

bolditalic:
  | s = STRING { String s }
;

inline(param):
  | s = STRING { String s }
  | p = param { p }
;

%%
