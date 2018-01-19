val sexpr_t : (module Alcotest.TESTABLE with type t = AST.sexpr)

val read_file : string -> string

val match_regexp_in_process_output : string -> Str.regexp -> string option

val test_parse : string -> AST.sexpr option

val codegen_sexpr : Codegen.env -> string -> Llvm.llvalue

val codegen_file : Codegen.env -> string -> Llvm.llvalue list

val create_obj : Codegen.env -> string -> string -> unit -> unit
