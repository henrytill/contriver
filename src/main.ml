(* Entry point *)

let flush_str out_channel str =
  output_string out_channel str;
  flush out_channel

let parse_with_error lexbuf =
  let open Contriver in
  try
    let ast = Parser.prog Lexer.read lexbuf in
    Ok (ast)
  with
  | Lexer.SyntaxError msg -> Error (Syntax msg)
  | Parser.Error          -> Error (Syntax "ill-formed expression")

let read_eval_print lexbuf out_channel err_channel env =
  let open Contriver in
  let out_formatter = Format.formatter_of_out_channel out_channel in
  let err_formatter = Format.formatter_of_out_channel err_channel in
  match parse_with_error lexbuf with
  | Error e ->
      lisp_error_printer err_formatter e;
      true
  | Ok(None)   ->
      false
  | Ok(Some v) ->
      begin
        match Evaluator.eval env v with
        | Ok v    -> lisp_value_printer out_formatter v
        | Error e -> lisp_error_printer err_formatter e
      end;
      true

let () =
  let continue = ref true in
  let in_channel = Pervasives.stdin in
  let out_channel = Pervasives.stdout in
  let err_channel = Pervasives.stderr in
  let lexbuf = Lexing.from_channel in_channel in
  let env = Evaluator.primitive_bindings in
  flush_str out_channel "Welcome to Contriver\n";
  try
    while !continue do
      flush_str out_channel "><> ";
      continue := read_eval_print lexbuf out_channel err_channel env
    done;
    raise End_of_file
  with
  | End_of_file ->
      flush_str out_channel "\nGoodbye!\n";
      exit 0
