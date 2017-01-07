type lisp_val = Atom of string
              | DottedList of lisp_val list * lisp_val
              | Vector of lisp_val array
              | Number of int
              | Float of float
              | Ratio of Ratio.ratio
              | Complex of Complex.t
              | String of string
              | Character of char
              | Bool of bool
              | PrimitiveFunc of (lisp_val list -> lisp_val)
              | Func of { func_params : string list;
                          func_varargs : string option;
                          func_body : lisp_val list;
                          func_closure : env
                        }

and env = (string * lisp_val) list ref

exception NumArgs        of int * lisp_val list
exception TypeMismatch   of string * lisp_val
exception BadSpecialForm of string * lisp_val
exception NotFunction    of string * string
exception UnboundVar     of string * string
exception Default        of string
