let flush_str out_channel str =
  output_string out_channel str;
  flush out_channel

let parse_with_error lexbuf =
  try
    let ast = Parser.prog Lexer.read lexbuf in
    Result.Ok (ast)
  with
  | Lexer.SyntaxError msg -> Result.Error ("lexer error: " ^ msg)
  | Parser.Error          -> Result.Error "parse error"

let read_eval_print lexbuf out_channel err_channel =
  let out_formatter = Format.formatter_of_out_channel out_channel in
  match parse_with_error lexbuf with
  | Result.Error e ->
      true
  | Result.Ok None ->
      false
  | Result.Ok (Some v) ->
      AST.Printer.sexpr_printer out_formatter v;
      true

let () =
  let interactive  = ref false in
  let continue     = ref true in
  let opt_list     = ["-i", Arg.Set interactive, "Run in interactive mode"] in
  let usage_string = "Usage: " ^ Sys.argv.(0) ^ " [options...]" in
  let in_channel   = Pervasives.stdin in
  let out_channel  = Pervasives.stdout in
  let err_channel  = Pervasives.stderr in
  let lexbuf       = Lexing.from_channel in_channel in
  try
    Arg.parse opt_list (fun _ -> ()) usage_string;
    if !interactive then flush_str out_channel "Welcome to contriver\n";
    while !continue do
      if !interactive then flush_str out_channel "><> ";
      continue := read_eval_print lexbuf out_channel err_channel
    done;
    raise End_of_file
  with
  | End_of_file ->
      if !interactive then flush_str out_channel "\nGoodbye!\n";
      exit 0
