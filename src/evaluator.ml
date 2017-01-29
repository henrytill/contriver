open Contriver

(* Unpackers *)

let unpack_bool : lisp_value -> bool throws_error = function
  | Bool b -> Ok b
  | x      -> Error (TypeMismatch ("bool", x))


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


(* Eval and friends *)

exception Eval_error of lisp_error

let rec eval env = function
  | String _ | Number _ | Bool _ as v                  -> Ok v
  | Atom x                                             -> get_var env x
  | List [Atom "quote"; v]                             -> Ok v
  | List [Atom "if"; predicate; consequent; alternate] -> if_exp env predicate consequent alternate
  | List (Atom "cond" :: clauses)                      -> cond_exp env clauses
  | List (Atom "case" :: key :: clauses)               -> case_exp env key clauses
  | List [Atom "set!";   Atom var; form]               -> eval env form >>= set_var env var
  | List [Atom "define"; Atom var; form]               -> eval env form >>= define_var env var
  | x                                                  -> Ok x

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
