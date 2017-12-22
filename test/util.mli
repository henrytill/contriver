val read_file : string -> string

val lisp_value_t : (module Alcotest.TESTABLE with type t = Contriver.lisp_value)

val lisp_error_t : (module Alcotest.TESTABLE with type t = Contriver.lisp_error)

val test_parse : string -> Contriver.lisp_value option

val test_eval : Contriver.env -> Contriver.lisp_value option -> Contriver.lisp_value Contriver.throws_error
