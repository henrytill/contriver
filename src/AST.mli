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

module Printer : sig

  val sexpr_printer : Format.formatter -> sexpr -> unit

  val list_of_sexprs_printer : Format.formatter -> sexpr list -> unit

end
