open AST

exception Error of string

let unwrap_atom = function
  | Atom a -> a
  | _      -> raise (Error "cannot unwrap")

let create_proto = function
  | Atom name :: params ->
      let params_arr = Array.of_list (List.map (fun a -> unwrap_atom a) params) in
      Prototype (name, params_arr)
  | _ ->
      raise (Error "invalid proto")

let rec sexpr_to_func = function
  | List [Atom "define"; List proto; body] ->
      Function (create_proto proto, sexpr_to_expr body)
  | _ ->
      raise (Error "unimplemented")

and sexpr_to_expr = function
  | List [Atom "+"; lhs; rhs] ->
      Binary ('+', sexpr_to_expr lhs, sexpr_to_expr rhs)
  | List [Atom "-"; lhs; rhs] ->
      Binary ('-', sexpr_to_expr lhs, sexpr_to_expr rhs)
  | List [Atom "*"; lhs; rhs] ->
      Binary ('*', sexpr_to_expr lhs, sexpr_to_expr rhs)
  | List [Atom "<"; lhs; rhs] ->
      Binary ('<', sexpr_to_expr lhs, sexpr_to_expr rhs)
  | List (Atom f :: args) ->
      let args_arr = Array.of_list (List.map (fun a -> sexpr_to_expr a) args) in
      Call (f, args_arr)
  | Atom a ->
      Variable a
  | Int i ->
      Number (float_of_int i)
  | Float f ->
      Number f
  | _ ->
      raise (Error "unimplemented")
