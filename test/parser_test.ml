open AST

let parse_single_atom () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (Atom "atom"))
    (Util.test_parse "atom")

let parse_quoted_symbol () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (List [Atom "quote"; Atom "atom"]))
    (Util.test_parse "'atom")

let parse_list_of_single_atom () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (List [Atom "bang"]))
    (Util.test_parse "(bang)")

let parse_function_application () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (List [Atom "+"; Int 12; Int 13]))
    (Util.test_parse "(+ 12 13)")

let parse_list_of_numbers () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (List [Atom "list"; Int 12; Int 13]))
    (Util.test_parse "(list 12 13)")

let parse_vector_of_numbers () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (Vector [|Int 12; Int 13|]))
    (Util.test_parse "#(12 13)")

let raise_for_bad_vector () =
  Alcotest.check_raises
    "floating hash"
    (Lexer.SyntaxError "Unexpected char: #")
    (fun () -> ignore (Util.test_parse "# (12 13)"))

let parse_quoted_list () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (List [Atom "quote"; List [Int 12; Int 13]]))
    (Util.test_parse "'(12 13)")

let parse_quoted_list_2 () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (List [Atom "quote"; List [Int 12; Int 13]]))
    (Util.test_parse "' (12 13)")

let parse_dotted_list () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (DottedList ([Int 12], Int 13)))
    (Util.test_parse "(12 . 13)")

let parse_dotted_list_2 () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (DottedList ([Int 12; Int 14], Int 13)))
    (Util.test_parse "(12 14 . 13)")

let number_test () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (Int 42))
    (Util.test_parse "42")

let float_test () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (Float 42.42))
    (Util.test_parse "42.42")

let string_test () =
  Alcotest.(check (option Util.sexpr_t))
    "same sexpr list"
    (Some (String "goliath"))
    (Util.test_parse "\"goliath\"")

let parser_set =
  [ "Parse a single atom",                                      `Quick, parse_single_atom
  ; "Parse a quoted symbol",                                    `Quick, parse_quoted_symbol
  ; "Parse a single atom in a list",                            `Quick, parse_list_of_single_atom
  ; "Parse a basic function application",                       `Quick, parse_function_application
  ; "Parse a list of numbers",                                  `Quick, parse_list_of_numbers
  ; "Parse a vector of numbers",                                `Quick, parse_vector_of_numbers
  ; "Raise a SyntaxError for an improperly constructed vector", `Quick, raise_for_bad_vector
  ; "Parse a quoted list of ints",                              `Quick, parse_quoted_list
  ; "Parse a quoted list of ints (2)",                          `Quick, parse_quoted_list_2
  ; "Parse a dotted list of ints",                              `Quick, parse_dotted_list
  ; "Parse a dotted list of ints (2)",                          `Quick, parse_dotted_list_2
  ; "Parse a number",                                           `Quick, number_test
  ; "Parse a float",                                            `Quick, float_test
  ; "Parse a string",                                           `Quick, string_test
  ]
