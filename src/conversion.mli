val create_proto : AST.sexpr list -> AST.proto

val sexpr_to_expr : AST.sexpr -> AST.expr

val sexpr_to_llvalue : Codegen.env -> AST.sexpr -> Llvm.llvalue
