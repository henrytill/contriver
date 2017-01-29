(* Entry point *)

let flush_str out_channel str =
  output_string out_channel str;
  flush out_channel

let read_prompt out_channel in_channel prompt : string =
  flush_str out_channel prompt;
  input_line in_channel

let eval_string out_channel env expr =
  let open Contriver in
  let formatter = Format.formatter_of_out_channel out_channel in
  match
    Syntax.read_expr_list Syntax.parse expr >>= Evaluator.eval_list env
  with
  | Ok(vs) ->
      list_of_lisp_values_printer formatter vs
  | Error(e) ->
      lisp_error_printer formatter e

let () =
  let continue = ref true in
  let in_channel = Pervasives.stdin in
  let out_channel = Pervasives.stdout in
  let env = ref [] in
  flush_str out_channel "Welcome to Contriver\n";
  try
    while !continue do
      let input = read_prompt out_channel in_channel "><> " in
      if input = ":quit" then
        continue := false
      else
        eval_string out_channel env input;
    done;
    raise End_of_file
  with
  | End_of_file ->
      flush_str out_channel "\nGoodbye!\n";
      exit 0
