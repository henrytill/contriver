type lisp_value =
  | Atom of string
  | List of lisp_value list
  | DottedList of lisp_value list * lisp_value
  | Vector of lisp_value array
  | Number of int
  | Float of float
  | Ratio of Num.num * Num.num
  | Complex of Complex.t
  | String of string
  | Character of char
  | Bool of bool
  | PrimitiveFunc of (lisp_value list -> lisp_value throws_error)
  | Func of func_t

and env = (string * lisp_value) list ref

and func_t =
  { func_params : string list;
    func_varargs : string option;
    func_body : lisp_value list;
    func_closure : env }

and lisp_error =
  | Syntax of string
  | NumArgs of int * lisp_value list
  | TypeMismatch of string * lisp_value
  | BadSpecialForm of string * lisp_value
  | NotFunction of string * string
  | UnboundVar of string * string
  | Default of string
  | Undefined

and ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

and 'a throws_error = ('a, lisp_error) result

val (>>=) : 'a throws_error -> ('a -> 'b throws_error) -> 'b throws_error

val show_lisp_value : lisp_value -> string

val show_list_of_lisp_values : lisp_value list -> string

val show_lisp_error : lisp_error -> string

val lisp_value_printer : Format.formatter -> lisp_value -> unit

val list_of_lisp_values_printer : Format.formatter -> lisp_value list -> unit

val lisp_error_printer : Format.formatter -> lisp_error -> unit