val the_module    : Llvm.llmodule

val codegen_expr  : AST.expr  -> Llvm.llvalue

val codegen_proto : AST.proto -> Llvm.llvalue

val codegen_func  : AST.func  -> Llvm.llvalue
