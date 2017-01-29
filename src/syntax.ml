open Contriver

let parse s : lisp_value list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let read_expr_list parser s : lisp_value list throws_error =
  try Ok (parser s)
  with
  | Lexer.SyntaxError msg -> Error (Syntax msg)
  | _                     -> Error (Syntax "ill-formed expression")
