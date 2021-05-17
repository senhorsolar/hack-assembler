%{
    open Ast
%}

%token <string> COMMENT
%token <string> ASYMBOL
%token <int> ADIGIT
%token <string option * string * string option> CINST
%token <string> LABEL
%token EOF

%start <Ast.expr> prog

%%

prog:
    | e = expr; EOF { e }
    ;

expr:
    | c = COMMENT { Comment c }
    | s = ASYMBOL { Asymbol s }
    | d = ADIGIT { Adigit d }
    | c = CINST { Cinst c }
    | l = LABEL { Label l }
    | e = expr; COMMENT {e}
    ;
