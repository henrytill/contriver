let () =
  let tests =
    [ "parser_set",  Parser_test.parser_set
    ; "codegen_set", Codegen_test.codegen_set
    ; "emit_set",    Emit_test.emit_set
    ]
  in
  Alcotest.run "Contriver" tests
