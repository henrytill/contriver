let evaluate_single_quoted_atom () =
  Alcotest.(check (result Util.lisp_value_t Util.lisp_error_t))
    "same lisp_value throws_error"
    (Result.Ok (Contriver.Atom "atom"))
    (Util.test_parse "'atom" |> Util.test_eval Evaluator.primitive_bindings)

let evaluate_string () =
  Alcotest.(check (result Util.lisp_value_t Util.lisp_error_t))
    "same lisp_value throws_error"
    (Result.Ok (Contriver.String "atom"))
    (Util.test_parse "\"atom\"" |> Util.test_eval Evaluator.primitive_bindings)

let evaluate_number () =
  Alcotest.(check (result Util.lisp_value_t Util.lisp_error_t))
    "same lisp_value throws_error"
    (Result.Ok (Contriver.Number 42))
    (Util.test_parse "42" |> Util.test_eval Evaluator.primitive_bindings)

let evaluator_set =
  [ "Evaluate a single quoted atom", `Quick, evaluate_single_quoted_atom
  ; "Evaluate a string",             `Quick, evaluate_string
  ; "Evaluate a number",             `Quick, evaluate_number
  ]
