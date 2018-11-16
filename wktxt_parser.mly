%{
  open Wktxt_type
%}

%token<int> HEADER LIST NUMLIST
%token<string> STRING
%token ITALIC BOLD
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
;

italic:
  | BOLD i = inline(bold)+ BOLD { Bold i }
;

bold:
  | ITALIC i = inline(italic)+ ITALIC { Italic i }
;

inline(param):
  | s = STRING { String s }
  | p = param { p }
;

%%
