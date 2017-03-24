let () =
  Alcotest.run "Contriver" [
    "parser_set",    Parser_test.parser_set;
  ]
