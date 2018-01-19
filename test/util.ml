exception Error of string

let sexpr_t =
  let module M = struct
    type t        = AST.sexpr
    let equal x y = x = y
    let pp        = AST.Printer.sexpr_printer
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

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

let match_regexp_in_process_output cmd regexp =
  let in_channel = Unix.open_process_in cmd in
  let rec search_loop ic r =
    try
      let line = input_line ic in
      if Str.string_match r line 0 then
        begin
          ignore (Unix.close_process_in ic);
          Some (Str.matched_group 1 line)
        end
      else
        search_loop ic r
    with
    | End_of_file ->
        ignore (Unix.close_process_in ic);
        None
    | exn ->
        ignore (Unix.close_process_in ic);
        raise exn
  in
  search_loop in_channel regexp

let test_parse s : AST.sexpr option =
  let lexbuf = Lexing.from_string s in
  let ast    = Parser.prog Lexer.read lexbuf in
  ast

let codegen_sexpr env sexpr =
  match test_parse sexpr with
  | Some ast ->
      Codegen.codegen_expr env (Conversion.sexpr_to_expr ast)
  | None ->
      raise (Error "Could not parse sexpr")

let codegen_file env file =
  let in_channel = open_in file in
  try
    let lexbuf = Lexing.from_channel in_channel in
    let rec loop exprs =
      match Parser.prog Lexer.read lexbuf with
      | Some ast ->
          loop (Conversion.sexpr_to_llvalue env ast :: exprs)
      | None ->
          List.rev exprs
    in
    let result = loop [] in
    ignore (close_in in_channel);
    result
  with
  | Lexer.SyntaxError msg ->
      ignore (close_in in_channel);
      raise (Error ("lexer error: " ^ msg))
  | Parser.Error ->
      ignore (close_in in_channel);
      raise (Error "parse error")

let create_obj env src dest () =
  let open Codegen in
  let env = create_env () in
  let _   = codegen_file env src in
  Emit.emit_module (get_module env) dest
