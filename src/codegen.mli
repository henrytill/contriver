type env

val create_env : unit -> env

val get_module : env -> Llvm.llmodule

val codegen_expr : env -> AST.expr  -> Llvm.llvalue

val codegen_proto : env -> AST.proto -> Llvm.llvalue

val codegen_func : env -> AST.func  -> Llvm.llvalue
