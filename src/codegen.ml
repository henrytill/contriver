exception Error of string

type env =
  { context            : Llvm.llcontext
  ; mutable the_module : Llvm.llmodule
  ; builder            : Llvm.llbuilder
  ; named_values       : (string, Llvm.llvalue) Hashtbl.t
  }

let create_env () =
  let ctxt = Llvm.global_context () in
  { context      = ctxt
  ; the_module   = Llvm.create_module ctxt "top-level"
  ; builder      = Llvm.builder ctxt
  ; named_values = Hashtbl.create 10
  }

let get_module env =
  env.the_module

let double_type env =
  Llvm.double_type env.context

let lookup_var table name =
  try
    Hashtbl.find table name
  with Not_found ->
    raise (Error "unknown variable name")

let rec codegen_expr env = function
  | AST.Number n                -> Llvm.const_float (double_type env) n
  | AST.Variable name           -> lookup_var env.named_values name
  | AST.Binary (op, lhs, rhs)   -> codegen_binary env op lhs rhs
  | AST.Call (callee, args)     -> codegen_call env callee args
  | AST.If (cond, then_, else_) -> codegen_if env cond then_ else_

and codegen_binary env op lhs rhs =
  let lhs_val = codegen_expr env lhs in
  let rhs_val = codegen_expr env rhs in
  begin
    match op with
    | '+' -> Llvm.build_fadd lhs_val rhs_val "addtmp" env.builder
    | '-' -> Llvm.build_fsub lhs_val rhs_val "subtmp" env.builder
    | '*' -> Llvm.build_fmul lhs_val rhs_val "multmp" env.builder
    | '<' ->
        (* Convert bool 0/1 to double 0.0 or 1.0 *)
        let i = Llvm.build_fcmp Llvm.Fcmp.Ult lhs_val rhs_val "cmptmp" env.builder in
        Llvm.build_uitofp i (double_type env) "booltmp" env.builder
    | _ -> raise (Error "invalid binary operator")
  end

and codegen_call env callee args =
  let callee =
    match Llvm.lookup_function callee env.the_module with
    | Some callee -> callee
    | None        -> raise (Error "unknown function referenced")
  in
  let params = Llvm.params callee in
  if Array.length params != Array.length args then raise (Error "incorrect # of arguments passed");
  let args = Array.map (codegen_expr env) args in
  Llvm.build_call callee args "calltmp" env.builder

and codegen_if env cond then_ else_ =
  let cond = codegen_expr env cond in
  (* Convert condition to a bool by comparing equal to 0.0 *)
  let zero     = Llvm.const_float (double_type env) 0.0 in
  let cond_val = Llvm.build_fcmp Llvm.Fcmp.One cond zero "ifcond" env.builder in
  (* Grab the first block so that we might later add the conditional branch
   * to it at the end of the function. *)
  let start_bb     = Llvm.insertion_block env.builder in
  let the_function = Llvm.block_parent start_bb in
  let then_bb      = Llvm.append_block env.context "then" the_function in
  (* Emit 'then' value *)
  Llvm.position_at_end then_bb env.builder;
  let then_val = codegen_expr env then_ in
  (* Codegen of 'then' can change the current block, update then_bb for the phi.
   * We create a new name because one is used for the phi node, and the
   * other is used for the conditional branch *)
  let new_then_bb = Llvm.insertion_block env.builder in
  (* Emit 'else' value. *)
  let else_bb = Llvm.append_block env.context "else" the_function in
  Llvm.position_at_end else_bb env.builder;
  let else_val = codegen_expr env else_ in
  (* Codegen of 'else' can change the current block, update else_bb for the
   * phi. *)
  let new_else_bb = Llvm.insertion_block env.builder in
  (* Emit merge block. *)
  let merge_bb = Llvm.append_block env.context "ifcont" the_function in
  Llvm.position_at_end merge_bb env.builder;
  let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
  let phi      = Llvm.build_phi incoming "iftmp" env.builder in
  (* Return to the start block to add the conditional branch. *)
  Llvm.position_at_end start_bb env.builder;
  ignore (Llvm.build_cond_br cond_val then_bb else_bb env.builder);
  (* Set a unconditional branch at the end of the 'then' block and the
   * 'else' block to the 'merge' block. *)
  Llvm.position_at_end new_then_bb env.builder;
  ignore (Llvm.build_br merge_bb env.builder);
  Llvm.position_at_end new_else_bb env.builder;
  ignore (Llvm.build_br merge_bb env.builder);
  (* Finally, set the builder to the end of the merge block. *)
  Llvm.position_at_end merge_bb env.builder;
  phi

let codegen_proto env = function
  | AST.Prototype (name, args) ->
      (* Make the function type: double(double, double) etc. *)
      let doubles = Array.make (Array.length args) (double_type env) in
      let ft      = Llvm.function_type (double_type env) doubles in
      let f =
        match Llvm.lookup_function name env.the_module with
        | None -> Llvm.declare_function name ft env.the_module
        | Some f ->
            if Array.length (Llvm.basic_blocks f) != 0 then
              raise (Error "redefinition of function");
            if Array.length (Llvm.params f) != Array.length args then
              raise (Error "redefinition of function with different # of args");
            f
      in
      Array.iteri (fun i a ->
          let n = args.(i) in
          Llvm.set_value_name n a;
          Hashtbl.add env.named_values n a)
        (Llvm.params f);
      f

let codegen_func env = function
  | AST.Function (proto, body) ->
      Hashtbl.clear env.named_values;
      let the_function = codegen_proto env proto in
      let bb           = Llvm.append_block env.context "entry" the_function in
      Llvm.position_at_end bb env.builder;
      try
        let ret_val = codegen_expr env body in
        ignore (Llvm.build_ret ret_val env.builder);
        Llvm_analysis.assert_valid_function the_function;
        the_function
      with e ->
        Llvm.delete_function the_function;
        raise e
