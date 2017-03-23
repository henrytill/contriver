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
%token HASH_LEFT_PAREN
%token QUOTE
%token UNQUOTE
%token QUASIQUOTE
%token DOT
%token EOF

%start <Contriver.lisp_value option> prog

%%

prog:
  | x = lisp_value { Some x }
  | EOF            { None   }
  ;

list_fields:
  | xs = list(lisp_value) { xs }
  ;

lisp_value:
  | LEFT_PAREN; xs = list_fields; RIGHT_PAREN                      { List xs                     }
  | LEFT_PAREN; xs = list_fields; DOT; x = lisp_value; RIGHT_PAREN { DottedList (xs, x)          }
  | HASH_LEFT_PAREN; xs = list_fields; RIGHT_PAREN                 { Vector (Array.of_list xs)   }
  | QUOTE; x = lisp_value                                          { List [Atom "quote"; x]      }
  | QUASIQUOTE; x = lisp_value                                     { List [Atom "quasiquote"; x] }
  | UNQUOTE; x = lisp_value                                        { List [Atom "unquote"; x]    }
  | a = ATOM                                                       { Atom a                      }
  | s = STRING                                                     { String s                    }
  | i = INT                                                        { Number i                    }
  | x = FLOAT                                                      { Float x                     }
  | TRUE                                                           { Bool true                   }
  | FALSE                                                          { Bool false                  }
  ;
