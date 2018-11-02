%{
    open Wktxt_type
%}

%token<int> HEADER
%token<string> STRING
%token<char> CHAR
%token EOF

%start document
%type <Wktxt_type.document> document

%%

document:
| EOF { [] }
| fragments EOF { $1 }
;

fragments:
| fragment { [$1] }
| fragment fragments { $1 :: $2 }
;

fragment:
| HEADER fragments { Header ($1, $2) }
| STRING { String $1 }
| CHAR { Char $1 }
;
