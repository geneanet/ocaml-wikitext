%{
    open Wktxt_type
%}

%token<int> HEADER
%token<string> STRING
%token<char> CHAR
%token EOF WHITE HEADER1 HEADER2 HEADER3 HEADER4 HEADER5 HEADER6

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
