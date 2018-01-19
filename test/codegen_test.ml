exception Error of string

let codegen_sexpr env sexpr =
  match Util.test_parse sexpr with
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

let test_file env name () =
  Alcotest.(check string)
    "same string"
    ("\n" ^ Util.read_file ("test-data/" ^ name ^ ".ll"))
    (codegen_file env ("test-data/" ^ name ^ ".tv") |> List.map Llvm.string_of_llvalue |> String.concat "")

let codegen_four_plus_five env () =
  Alcotest.(check string)
    "same string"
    "double 9.000000e+00"
    (codegen_sexpr env "(+ 4 5)" |> Llvm.string_of_llvalue)

let codegen_set =
  let env = Codegen.create_env () in
  [ "Codegen 4+5",         `Quick, codegen_four_plus_five env
  ; "Codegen average",     `Quick, test_file env "average"
  ; "Codegen conditional", `Quick, test_file env "conditional"
  ; "Codegen t",           `Quick, test_file env "t"
  ]
