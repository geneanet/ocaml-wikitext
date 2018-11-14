%{
  open Wktxt_type
%}

%token<int> HEADER
%token<string> STRING
%token<char> CHAR
%token ITALIC BOLD
%token EOF WHITE

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
