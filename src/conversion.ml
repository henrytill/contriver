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

let rec sexpr_to_expr = function
  | List [Atom "+"; lhs; rhs] ->
      Binary ('+', sexpr_to_expr lhs, sexpr_to_expr rhs)
  | List [Atom "-"; lhs; rhs] ->
      Binary ('-', sexpr_to_expr lhs, sexpr_to_expr rhs)
  | List [Atom "*"; lhs; rhs] ->
      Binary ('*', sexpr_to_expr lhs, sexpr_to_expr rhs)
  | List [Atom "<"; lhs; rhs] ->
      Binary ('<', sexpr_to_expr lhs, sexpr_to_expr rhs)
  | List [Atom "if"; predicate; consequent; alternate] ->
      If (sexpr_to_expr predicate, sexpr_to_expr consequent, sexpr_to_expr alternate)
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

let sexpr_to_llvalue env = function
  | List [Atom "define"; List proto; body] ->
      let func = Function (create_proto proto, sexpr_to_expr body) in
      Codegen.codegen_func env func
  | List [Atom "extern"; List proto] ->
      let proto = create_proto proto in
      Codegen.codegen_proto env proto
  | _ ->
      raise (Error "unimplemented")
