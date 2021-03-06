{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int     = ('-'? ['0'-'9'] ['0'-'9']*)
let digit   = ['0'-'9']
let frac    = ('.' digit*)
let exp     = (['e' 'E'] ['-' '+']? digit+)
let float   = digit* frac? exp?
let symbol  = ['!' '$' '%' '&' '|' '*' '+' '-' '/' ':' '<' '=' '>' '?' '@' '^' '_' '~']
let white   = ([' ' '\t']+)
let newline = '\r' | '\n' | "\r\n"
let id      = (['a'-'z' 'A'-'Z' '_'] | symbol) ((['a'-'z' 'A'-'Z' '0'-'9' '_'] | symbol)*)
let comment = (';' (_ # ['\r' '\n'])*)

rule read =
  parse
  | white    { read lexbuf }
  | comment  { next_line lexbuf; read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | id       { ATOM (Lexing.lexeme lexbuf) }
  | "#t"     { TRUE }
  | "#f"     { FALSE }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | '#' '('  { HASH_LEFT_PAREN }
  | '\''     { QUOTE }
  | '`'      { QUASIQUOTE }
  | ','      { UNQUOTE }
  | '.'      { DOT }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_string buf =
  parse
  | '"'           { STRING (Buffer.contents buf) }
  | '\\' '/'      { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'     { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'      { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'      { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'      { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'      { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'      { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _             { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (SyntaxError ("String is not terminated")) }
