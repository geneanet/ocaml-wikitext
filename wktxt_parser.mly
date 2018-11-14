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
  | h = HEADER f = inline+ { Header (h, f) }
;

(* inlines *)

inline:
  | ITALIC i = inline+ ITALIC { Italic i }
  | BOLD i = inline+ BOLD { Bold i }
  | s = STRING { String s }
  | c = CHAR { Char c }
  | WHITE { White }
;

%%
