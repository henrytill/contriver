open Contriver

let lisp_value_t =
  let module M = struct
    type t = lisp_value list
    let equal xs ys = xs = ys
    let pp ppf xs = Format.pp_print_string ppf (show_list_of_lisp_values xs)
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let raw_atom_test () =
  Alcotest.(check lisp_value_t)
    "same lisp_value"
    [Atom "foo"]
    (Syntax.parse "foo")

let list_atom_test_01 () =
  Alcotest.(check lisp_value_t)
    "same lisp_value"
    [List [Atom "bang"]]
    (Syntax.parse "(bang)")

let list_atom_test_02 () =
  Alcotest.(check lisp_value_t)
    "same lisp_value"
    [List [Atom "plus"; Number 12; Number 13]]
    (Syntax.parse "(plus 12 13)")

let number_test () =
  Alcotest.(check lisp_value_t)
    "same lisp_value"
    [Number 42]
    (Syntax.parse "42")

let float_test () =
  Alcotest.(check lisp_value_t)
    "same lisp_value"
    [Float 42.42]
    (Syntax.parse "42.42")

let string_test () =
  Alcotest.(check lisp_value_t)
    "same lisp_value"
    [String "goliath"]
    (Syntax.parse "\"goliath\"")

let parser_set = [
  "Parse a raw atom",             `Quick, raw_atom_test;
  "Parse an atom in a list (01)", `Quick, list_atom_test_01;
  "Parse an atom in a list (02)", `Quick, list_atom_test_02;
  "Parse a number",               `Quick, number_test;
  "Parse a float",                `Quick, float_test;
  "Parse a string",               `Quick, string_test
]

let () =
  Alcotest.run "Parser" ["parser_set", parser_set;]
