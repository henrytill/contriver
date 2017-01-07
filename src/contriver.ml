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
  | Func of { func_params : string list;
              func_varargs : string option;
              func_body : lisp_value list;
              func_closure : env }

and env = (string * lisp_value) list ref

and lisp_error =
  | NumArgs of int * lisp_value list
  | TypeMismatch of string * lisp_value
  | BadSpecialForm of string * lisp_value
  | NotFunction of string * string
  | UnboundVar of string * string
  | Default of string

and 'a throws_error = ('a, lisp_error) result



let unwords = String.concat " "

let rec show_lisp_value : lisp_value -> string = function
  | Atom name          -> name
  | List xs            -> "(" ^ unwordsList xs ^ ")"
  | DottedList (xs, x) -> "(" ^ unwordsList xs ^ " . " ^ show_lisp_value x ^ ")"
  | Vector xs          -> "#(" ^ unwordsList (Array.to_list xs) ^ ")"
  | Number x           -> string_of_int x
  | Float x            -> string_of_float x
  | Ratio (x, y)       -> Num.string_of_num x ^ "/" ^ Num.string_of_num y
  | Complex x          -> let open Complex in string_of_float (x.re) ^ "+" ^ string_of_float (x.im)
  | String x           -> "\"" ^ x ^ "\""
  | Character c        -> "#\\" ^ String.make 1 c
  | Bool true          -> "#t"
  | Bool false         -> "#f"
  | PrimitiveFunc _    -> "<primitive>"
  | Func { func_params = ps; func_varargs = vs } ->
    "(lambda ("
    ^ unwords ps
    ^ (match vs with
        | Some arg -> " . " ^ arg
        | None     -> "")
    ^ ") ...)"

and unwordsList xs = List.map show_lisp_value xs |> unwords

let show_lisp_error : lisp_error -> string = function
  | NumArgs (expected, found)      -> "Expected " ^ string_of_int expected ^ "args: found values " ^ unwordsList found
  | TypeMismatch (expected, found) -> "Invalid type: expected " ^ expected ^ " value, found " ^ show_lisp_value found
  | BadSpecialForm (message, form) -> message ^ ": " ^ show_lisp_value form
  | NotFunction (message, func)    -> message ^ ": " ^ func
  | UnboundVar (message, varname)  -> message ^ ": " ^ varname
  | Default message                -> "Default error: " ^ message

let lisp_value_printer fmt v =
  Format.fprintf fmt "%s" (show_lisp_value v)

let lisp_error_printer fmt e =
  Format.fprintf fmt "%s" (show_lisp_error e)

let () = print_endline "nothing yet"
