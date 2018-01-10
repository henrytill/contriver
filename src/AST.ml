type sexpr =
  | List of sexpr list
  | DottedList of sexpr list * sexpr
  | Vector of sexpr array
  | Atom of string
  | String of string
  | Int of int
  | Float of float
  | Bool of bool

type expr =
  | Number of float
  | Variable of string
  | Binary of char * expr * expr
  | Call of string * expr array
  | If of expr * expr * expr

type proto = Prototype of string * string array

type func = Function of proto * expr

module Printer = struct

  let unwords =
    String.concat " "

  let rec show_sexpr = function
    | List xs            -> "("  ^ show_list_of_sexprs xs                        ^ ")"
    | DottedList (xs, x) -> "("  ^ show_list_of_sexprs xs ^ " . " ^ show_sexpr x ^ ")"
    | Vector xs          -> "#(" ^ show_list_of_sexprs (Array.to_list xs)        ^ ")"
    | Atom name          -> name
    | String x           -> "\"" ^ x ^ "\""
    | Int x              -> string_of_int x
    | Float x            -> string_of_float x
    | Bool true          -> "#t"
    | Bool false         -> "#f"

  and show_list_of_sexprs xs =
    List.map show_sexpr xs |> unwords

  let sexpr_printer fmt v =
    Format.fprintf fmt "%s" (show_sexpr v);
    Format.pp_print_newline fmt ();
    Format.pp_print_flush fmt ()

  let list_of_sexprs_printer fmt vs =
    (Format.pp_print_list sexpr_printer) fmt vs;
    Format.pp_print_newline fmt ();
    Format.pp_print_flush fmt ()

end
