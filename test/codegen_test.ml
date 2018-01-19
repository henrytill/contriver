let test_file env name () =
  Alcotest.(check string)
    "same string"
    ("\n" ^ Util.read_file ("test-data/" ^ name ^ ".ll"))
    (Util.codegen_file env ("test-data/" ^ name ^ ".tv")
     |> List.map Llvm.string_of_llvalue
     |> String.concat "")

let codegen_four_plus_five env () =
  Alcotest.(check string)
    "same string"
    "double 9.000000e+00"
    (Util.codegen_sexpr env "(+ 4 5)" |> Llvm.string_of_llvalue)

let codegen_set =
  let env = Codegen.create_env () in
  [ "Codegen 4+5",         `Quick, codegen_four_plus_five env
  ; "Codegen average",     `Quick, test_file env "average"
  ; "Codegen conditional", `Quick, test_file env "conditional"
  ; "Codegen t",           `Quick, test_file env "t"
  ]
