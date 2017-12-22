let () =
  let tests =
    [ "parser_set",    Parser_test.parser_set
    ; "evaluator_set", Evaluator_test.evaluator_set
    ]
  in
  Alcotest.run "Contriver" tests
