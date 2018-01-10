let average =
  let open AST in
  Function (Prototype ("average", [|"x"; "y"|]),
            Binary ('*', Binary ('+', Variable "x", Variable "y"), Number 0.5))

let () =
  let open Codegen in
  let env = create_env () in
  let _   = codegen_func env average in
  Emit.emit_module (get_module env) Sys.argv.(1)
