%{
open AST
%}

%token <string> ATOM
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token TRUE
%token FALSE
%token LEFT_PAREN
%token RIGHT_PAREN
%token HASH_LEFT_PAREN
%token QUOTE
%token UNQUOTE
%token QUASIQUOTE
%token DOT
%token EOF

%start <AST.sexpr option> prog

%%

prog:
  | x = sexpr { Some x }
  | EOF            { None   }
  ;

list_fields:
  | xs = list(sexpr) { xs }
  ;

sexpr:
  | LEFT_PAREN; xs = list_fields; RIGHT_PAREN                 { List xs                     }
  | LEFT_PAREN; xs = list_fields; DOT; x = sexpr; RIGHT_PAREN { DottedList (xs, x)          }
  | HASH_LEFT_PAREN; xs = list_fields; RIGHT_PAREN            { Vector (Array.of_list xs)   }
  | QUOTE; x = sexpr                                          { List [Atom "quote"; x]      }
  | QUASIQUOTE; x = sexpr                                     { List [Atom "quasiquote"; x] }
  | UNQUOTE; x = sexpr                                        { List [Atom "unquote"; x]    }
  | a = ATOM                                                  { Atom a                      }
  | s = STRING                                                { String s                    }
  | i = INT                                                   { Int i                       }
  | x = FLOAT                                                 { Float x                     }
  | TRUE                                                      { Bool true                   }
  | FALSE                                                     { Bool false                  }
  ;
