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
  | h1 = HEADER i = inline+ HEADER { 
      Header (h1, i)
    }
  | l = LIST i = inline+ {
      List (l, i)
  }
  | l = NUMLIST i = inline+ {
      Num_list (l, i)
  }
  | HRULE { Hrule }
  | EMPTYLINE { Emptyline } (* do not match alone, use it in others*)
  | i = inline+ { Paragraph i }
;

(* inlines *)

inline:
  | ITALIC i = inline+ ITALIC { Italic i }
  | BOLD i = inline+ BOLD { Bold i }
  | s = STRING { String s }
  | c = CHAR { Char c }
;

%%
