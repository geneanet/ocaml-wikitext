%{
  open Wktxt_type
%}

%token<int> HEADER
%token<string> STRING
%token<char> CHAR
%token EOF WHITE

%start document
%type <Wktxt_type.document> document

%%

document:
| EOF { [] }
| fragment+ EOF { $1 }
;

fragment:
| h = HEADER f = fragment+ { Header (h, f) }
| s = STRING { String s }
| c = CHAR { Char c }
| WHITE { White }
;
