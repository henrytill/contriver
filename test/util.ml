let read_file file =
  let ic  = open_in file in
  let buf = Buffer.create (in_channel_length ic) in
  try
    while true do
      let line = input_line ic in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n'
    done;
    assert false
  with End_of_file ->
    close_in ic;
    Buffer.contents buf

let lisp_value_t =
  let module M = struct
    type t        = Contriver.lisp_value
    let equal x y = x = y
    let pp        = Contriver.lisp_value_printer
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let lisp_error_t =
  let module M = struct
    type t        = Contriver.lisp_error
    let equal x y = x = y
    let pp        = Contriver.lisp_error_printer
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let test_parse s : Contriver.lisp_value option =
  let lexbuf = Lexing.from_string s in
  let ast    = Parser.prog Lexer.read lexbuf in
  ast

let test_eval env = function
  | None   -> Result.Error (Contriver.Syntax "Could not parse expression")
  | Some v -> Evaluator.eval env v
