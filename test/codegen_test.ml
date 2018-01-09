let env = Codegen.create_env ()

let four_plus_five = AST.Binary ('+', AST.Number 4.0, AST.Number 5.0)

let codegen_four_plus_five () =
  Alcotest.(check string)
    "same string"
    "double 9.000000e+00"
    (let gen = Codegen.codegen_expr env four_plus_five in Llvm.string_of_llvalue gen)

let codegen_set =
  [ "Codegen 4+5", `Quick, codegen_four_plus_five
  ]
