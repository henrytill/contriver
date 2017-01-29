open Contriver

let lisp_value_t =
  let module M = struct
    type t = lisp_value
    let equal x y = x = y
    let pp = lisp_value_printer
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let parse_single_atom () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [Atom "atom"]
    (Syntax.parse "atom")

let parse_quoted_symbol () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [List [Atom "quote"; Atom "atom"]]
    (Syntax.parse "'atom")

let parse_list_of_single_atom () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [List [Atom "bang"]]
    (Syntax.parse "(bang)")

let parse_function_application () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [List [Atom "+"; Number 12; Number 13]]
    (Syntax.parse "(+ 12 13)")

let parse_list_of_numbers () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [List [Atom "list"; Number 12; Number 13]]
    (Syntax.parse "(list 12 13)")

let parse_vector_of_numbers () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [Vector [|Number 12; Number 13|]]
    (Syntax.parse "#(12 13)")

let raise_for_bad_vector () =
  Alcotest.check_raises
    "floating hash"
    (Lexer.SyntaxError "Unexpected char: #")
    (fun () -> ignore (Syntax.parse "# (12 13)"))

let parse_quoted_list () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [List [Atom "quote"; List [Number 12; Number 13]]]
    (Syntax.parse "'(12 13)")

let parse_quoted_list_2 () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [List [Atom "quote"; List [Number 12; Number 13]]]
    (Syntax.parse "' (12 13)")

let parse_dotted_list () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [DottedList ([Number 12], Number 13)]
    (Syntax.parse "(12 . 13)")

let parse_dotted_list_2 () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [DottedList ([Number 12; Number 14], Number 13)]
    (Syntax.parse "(12 14 . 13)")

let number_test () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [Number 42]
    (Syntax.parse "42")

let float_test () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [Float 42.42]
    (Syntax.parse "42.42")

let string_test () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [String "goliath"]
    (Syntax.parse "\"goliath\"")

let parse_multiple_expressions () =
  Alcotest.(check (list lisp_value_t))
    "same lisp_value list"
    [
      Vector [|Number 12; Number 13|];
      List [Atom "quasiquote"; List [List [Atom "unquote"; Atom "a"]; Number 2]]
    ]
    (Syntax.parse "#(12 13)\n `(,a 2)")

let parser_set = [
  "Parse a single atom",                                      `Quick, parse_single_atom;
  "Parse a quoted symbol",                                    `Quick, parse_quoted_symbol;
  "Parse a single atom in a list",                            `Quick, parse_list_of_single_atom;
  "Parse a basic function application",                       `Quick, parse_function_application;
  "Parse a list of numbers",                                  `Quick, parse_list_of_numbers;
  "Parse a vector of numbers",                                `Quick, parse_vector_of_numbers;
  "Raise a SyntaxError for an improperly constructed vector", `Quick, raise_for_bad_vector;
  "Parse a quoted list of numbers",                           `Quick, parse_quoted_list;
  "Parse a quoted list of numbers (2)",                       `Quick, parse_quoted_list_2;
  "Parse a dotted list of numbers",                           `Quick, parse_dotted_list;
  "Parse a dotted list of numbers (2)",                       `Quick, parse_dotted_list_2;
  "Parse a number",                                           `Quick, number_test;
  "Parse a float",                                            `Quick, float_test;
  "Parse a string",                                           `Quick, string_test;
  "Parse multiple expressions",                               `Quick, parse_multiple_expressions
]

let () =
  Alcotest.run "Parser" ["parser_set", parser_set;]
