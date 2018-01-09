type env =
  { context            : Llvm.llcontext
  ; mutable the_module : Llvm.llmodule
  ; builder            : Llvm.llbuilder
  ; named_values       : (string, Llvm.llvalue) Hashtbl.t
  }

val create_env : unit -> env

val codegen_expr : env -> AST.expr  -> Llvm.llvalue

val codegen_proto : env -> AST.proto -> Llvm.llvalue

val codegen_func : env -> AST.func  -> Llvm.llvalue
