let () =
  let tests = ["parser_set", Parser_test.parser_set] in
  Alcotest.run "Contriver" tests
