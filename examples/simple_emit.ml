let average =
  let open AST in
  Function (Prototype ("average", [|"x"; "y"|]),
            Binary ('*', Binary ('+', Variable "x", Variable "y"), Number 0.5))

let () =
  let _ = Codegen.codegen_func average in
  Emit.emit_module Codegen.the_module Sys.argv.(1)
