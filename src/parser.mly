%{
open Contriver
%}

%token <string> ATOM
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token TRUE
%token FALSE
%token LEFT_PAREN
%token RIGHT_PAREN
%token EOF

%start <Contriver.lisp_value list> prog

%%

prog:
  | xs = list_fields; EOF { xs }
  ;

list_fields:
  | xs = list(lisp_value) { xs }
  ;

lisp_value:
  | LEFT_PAREN; vl = list_fields; RIGHT_PAREN { List vl    }
  | a = ATOM                                  { Atom a     }
  | s = STRING                                { String s   }
  | i = INT                                   { Number i   }
  | x = FLOAT                                 { Float x    }
  | TRUE                                      { Bool true  }
  | FALSE                                     { Bool false }
  ;
