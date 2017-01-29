open Contriver

(* List primitives *)

let zip xs ys =
  let rec loop xs ys acc =
    match (xs, ys) with
    | [], _ | _, []        -> List.rev acc
    | (l :: ls), (r :: rs) -> loop ls rs ((l, r) :: acc)
  in
  loop xs ys []

let eq_pair f (x, y) : bool =
  match f [x; y] with
  | Error _     -> false
  | Ok (Bool v) -> v
  | Ok z        -> raise (Failure "eq_pair: this should never happen")

let eq_list f : lisp_value list -> lisp_value throws_error  = function
  | [List x; List y] -> Ok (Bool (List.length x = List.length y && List.for_all (eq_pair f) (zip x y)))
  | _                -> raise (Failure "eq_list: this should never happen")

let rec eqv : lisp_value list -> lisp_value throws_error = function
  | [Bool x; Bool y]                         -> Ok (Bool (x = y))
  | [Number x; Number y]                     -> Ok (Bool (x = y))
  | [Float x; Float y]                       -> Ok (Bool (x = y))
  | [Ratio (a, b); Ratio (c, d)]             -> Ok (Bool (let open Num in eq_num (div_num a b) (div_num c d)))
  | [Complex x; Complex y]                   -> Ok (Bool (x = y))
  | [String x; String y]                     -> Ok (Bool (x = y))
  | [Atom x; Atom y]                         -> Ok (Bool (x = y))
  | [(List _) as l1; (List _) as l2]         -> eq_list eqv [l1; l2]
  | [DottedList (xs, x); DottedList (ys, y)] -> eq_list eqv [List (xs @ [x]); List (ys @ [y])]
  | [_; _]                                   -> Ok (Bool false)
  | x                                        -> Error (NumArgs (2, x))


(* Unpackers *)

type 'a unpacker = lisp_value -> 'a throws_error

let unpack_num : int unpacker = function
  | Number n -> Ok(n)
  | x        -> Error (TypeMismatch ("number", x))

let unpack_bool : bool unpacker = function
  | Bool b -> Ok b
  | x      -> Error (TypeMismatch ("bool", x))

let unpack_str : string unpacker = function
  | String s -> Ok s
  | x        -> Error (TypeMismatch ("string", x))

let unpack_equals a b (unpacker : 'a unpacker) =
  unpacker a >>= fun upa ->
  unpacker b >>= fun upb ->
  Ok (upa = upb)


(* Environment handling *)
let get_var env var =
  try
    Ok (List.assoc var !env)
  with Not_found ->
    Error (UnboundVar ("Getting an unbound variable", var))

let set_var env var value =
  try
    let _ = List.assoc var !env in
    env := (var, value) :: List.remove_assoc var !env;
    Ok value
  with Not_found ->
    Error (UnboundVar ("Setting an unbound variable", var))

let define_var env var value =
  env := (var, value) :: List.remove_assoc var !env;
  Ok value

let bind_vars env bindings =
  List.iter (fun (var, value) ->
      env := (var, value) :: List.remove_assoc var !env)
    bindings;
  env


(* Helper functions *)

exception Found_match
exception Found_error of lisp_error

let contains key xs =
  try
    List.iter (fun x ->
        match eqv [key; x] with
        | Ok (Bool false) -> ()
        | Ok (Bool true)  -> raise Found_match
        | Ok _            -> raise (Failure "find_match: this should never happen")
        | Error e         -> raise (Found_error e))
      xs;
    Ok (Bool false)
  with
  | Found_match   -> Ok (Bool true)
  | Found_error e -> Error e

let rec last = function
  | []      -> Ok (List [])
  | x :: [] -> Ok x
  | _ :: xs -> last xs

let rec drop n xs =
  match xs with
  | []      -> []
  | _ :: tl -> if n = 0 then xs else drop (n - 1) tl


(* Functions *)

let make_func varargs env params body =
  let closure = ref [] in
  let ps = List.map show_lisp_value params in
  closure := !env;
  Ok (Func { func_params  = ps;
             func_varargs = varargs;
             func_body    = body;
             func_closure = closure; })

let make_normal_func =
  make_func None

let make_varargs x =
  make_func (Some (show_lisp_value x))

let isNone = function
  | Some _ -> false
  | None   -> true


(* Unary Operations *)

let symbolp = function
  | Atom _ -> Bool true
  | _      -> Bool false

let numberp = function
  | Number _ -> Bool true
  | _        -> Bool false

let stringp = function
  | String _ -> Bool true
  | _        -> Bool false

let boolp = function
  | Bool _ -> Bool true
  | _      -> Bool false

let listp = function
  | List _       -> Bool true
  | DottedList _ -> Bool true
  | _            -> Bool false

let symbol_to_string = function
  | Atom s -> String s
  | _      -> String ""

let string_to_symbol = function
  | String s -> Atom s
  | _        -> Atom ""


(* List primitives *)

let car = function
  | [List (x :: _)]          -> Ok x
  | [DottedList (x :: _, _)] -> Ok x
  | [x]                      -> Error (TypeMismatch ("pair", x))
  | x                        -> Error (NumArgs (1, x))

let cdr = function
  | [List (_ :: xs)]               -> Ok (List xs)
  | [DottedList (_ :: s :: xs, x)] -> Ok (DottedList (s :: xs, x))
  | [DottedList ([_], x)]          -> Ok x
  | [x]                            -> Error (TypeMismatch ("pair", x))
  | x                              -> Error (NumArgs (1, x))

let cons = function
  | [h; List []]            -> Ok (List [h])
  | [h; List xs]            -> Ok (List (h :: xs))
  | [h; DottedList (xs, x)] -> Ok (DottedList (h :: xs, x))
  | [h; x]                  -> Ok (DottedList ([h], x))
  | h                       -> Error (NumArgs (2, h))


(* Eval and friends *)

exception Eval_error of lisp_error

let numeric_bin_op op = function
  | [] as param ->
      Error (NumArgs (2, param))
  | hd :: tl    ->
      List.fold_left
        (fun acc c ->
           match acc, c with
           | Ok (Number x), Number y -> Ok (Number (op x y))
           | _,             _        -> raise (Failure "numeric_bin_op: this should never happen"))
        (Ok hd)
        tl

let unary_op f = function
  | [v] -> Ok (f v)
  | x   -> Error (NumArgs (1, x))

let bool_bin_op unpacker op = function
  | [a; b] ->
      unpacker a >>= fun l ->
      unpacker b >>= fun r ->
      Ok (Bool (op l r))
  | x ->
      Error (NumArgs (1, x))

let num_bool_bin_op =
  bool_bin_op unpack_num

let str_bool_bin_op =
  bool_bin_op unpack_str

let bool_bool_bin_op =
  bool_bin_op unpack_bool

let primitives =
  [ ("+",              numeric_bin_op (+))
  ; ("-",              numeric_bin_op (-))
  ; ("*",              numeric_bin_op ( * ))
  ; ("/",              numeric_bin_op (/))
  ; ("mod",            numeric_bin_op (mod))
  ; ("symbol?",        unary_op symbolp)
  ; ("number?",        unary_op numberp)
  ; ("string?",        unary_op stringp)
  ; ("bool?",          unary_op boolp)
  ; ("list?",          unary_op listp)
  ; ("symbol->string", unary_op symbol_to_string)
  ; ("string->symbol", unary_op string_to_symbol)
  ; ("=",              num_bool_bin_op (==))
  ; ("<",              num_bool_bin_op (<))
  ; (">",              num_bool_bin_op (>))
  ; ("/=",             num_bool_bin_op (<>))
  ; (">=",             num_bool_bin_op (>=))
  ; ("<=",             num_bool_bin_op (<=))
  ; ("&&",             bool_bool_bin_op (&&))
  ; ("||",             bool_bool_bin_op (||))
  ; ("string=?",       str_bool_bin_op (==))
  ; ("string<?",       str_bool_bin_op (<))
  ; ("string>?",       str_bool_bin_op (>))
  ; ("string<=?",      str_bool_bin_op (<=))
  ; ("string>=?",      str_bool_bin_op (>=))
  ; ("car",            car)
  ; ("cdr",            cdr)
  ; ("eqv?",           eqv)
  ; ("eq?",            eqv)
  ]

let primitive_bindings =
  let bindings = ref [] in
  let make_prim_func (var, func) = var, PrimitiveFunc func in
  let all_primitives = List.map make_prim_func primitives in
  bindings := all_primitives @ !bindings;
  bindings

let rec eval env = function
  | String _ | Number _ | Bool _ as v ->
      Ok v
  | Atom x ->
      get_var env x
  | List [Atom "quote"; v] ->
      Ok v
  | List [Atom "if"; predicate; consequent; alternate] ->
      if_exp env predicate consequent alternate
  | List (Atom "cond" :: clauses) ->
      cond_exp env clauses
  | List (Atom "case" :: key :: clauses) ->
      case_exp env key clauses
  | List [Atom "set!";   Atom var; form] ->
      eval env form >>= set_var env var
  | List [Atom "define"; Atom var; form] ->
      eval env form >>= define_var env var
  | List (Atom "define" :: List (Atom var :: params) :: body) ->
      make_normal_func env params body >>= define_var env var
  | List (Atom "define" :: DottedList ((Atom var :: params), varargs) :: body) ->
      make_varargs varargs env params body >>= define_var env var
  | List (Atom "lambda" :: List params :: body) ->
      make_normal_func env params body
  | List (Atom "lambda" :: DottedList (params, varargs) :: body)->
      make_varargs varargs env params body
  | List (Atom "lambda" :: (Atom _ as varargs) :: body) ->
      make_varargs varargs env [] body
  | List (f :: args) ->
      eval env f         >>= fun func ->
      eval_list env args >>= fun arg_vals ->
      apply func arg_vals
  | bad_form ->
      Error (BadSpecialForm ("Unrecognized special form", bad_form))

and eval_list env xs =
  try
    List.map
      (fun x ->
         match eval env x with
         | Ok v    -> v
         | Error e -> raise (Eval_error e))
      xs
    |> (fun xs -> Ok xs)
  with Eval_error e ->
    Error e

and if_exp env predicate consequent alternate =
  let g = function
    | Bool true  -> eval env consequent
    | Bool false -> eval env alternate
    | x          -> Error (TypeMismatch ("bool", x))
  in
  eval env predicate >>= g

and cond_exp env = function
  | [List [Atom "else"; consequent]]   -> eval env consequent
  | List [predicate; consequent] :: xs ->
      let g = function
        | Bool true  -> eval env consequent
        | Bool false -> cond_exp env xs
        | x          -> Error (TypeMismatch ("bool" ,x))
      in
      eval env predicate >>= g
  | x -> Error (NumArgs (1, x))

and case_exp env key = function
  | List (Atom "else" :: then_body) :: _ ->
      eval_list env then_body >>= last
  | List (List datums :: then_body) :: clauses ->
      eval env key           >>= fun result      ->
      contains result datums >>= fun found_match ->
      if found_match = Bool true then
        eval_list env then_body >>= last
      else
        case_exp env key clauses
  | [] ->
      Error Undefined
  | clauses ->
      Error (BadSpecialForm ("Ill-formed case expression", List (Atom "case" :: key :: clauses)))

and apply func args =
  match func with
  | PrimitiveFunc f ->
      f args
  | Func { func_params; func_varargs; func_body; func_closure } ->
      let num = List.length in
      if (num func_params) <> (num args) && isNone func_varargs then
        Error (NumArgs (num func_params, args))
      else
        begin
          let remaining_args = drop (num func_params) args in
          let bind_varargs args env =
            match args with
            | Some arg_name -> bind_vars env [arg_name, List remaining_args]
            | None          -> env
          in
          let eval_body bod env = eval_list env bod >>= last in
          bind_vars func_closure (zip func_params args)
          |> bind_varargs func_varargs
          |> eval_body func_body
        end
  | x ->
      Error (TypeMismatch ("func", x))
