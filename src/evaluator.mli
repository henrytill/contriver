open Contriver

val eval : env -> lisp_value -> lisp_value throws_error

val eval_list : env -> lisp_value list -> lisp_value list throws_error

val primitive_bindings : env
