%{
  open Wktxt_type
%}

%token<int> HEADER LIST NUMLIST
%token<string> STRING
%token<char> CHAR
%token ITALIC BOLD
%token EOF HRULE EMPTYLINE

%start document
%type <Wktxt_type.document> document

%%

document:
  | EOF { [] }
  | block+ EOF { $1 }
;

block:
  | h1 = HEADER i = inline+ HEADER EMPTYLINE* { 
      Header (h1, i)
    }
  | l = LIST i = inline+ EMPTYLINE* {
      List (l, i)
  }
  | l = NUMLIST i = inline+ EMPTYLINE* {
      Num_list (l, i)
  }
  | HRULE EMPTYLINE* { Hrule }
  | i = inline+ EMPTYLINE* { Paragraph i }
;

(* inlines *)

inline:
  | ITALIC i = inline+ ITALIC { Italic i }
  | BOLD i = inline+ BOLD { Bold i }
  | s = STRING { String s }
  | c = CHAR { Char c }
;

%%
