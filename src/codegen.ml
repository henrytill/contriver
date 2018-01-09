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

let double_type env =
  Llvm.double_type env.context

let lookup_var table name =
  try
    Hashtbl.find table name
  with Not_found ->
    raise (Error "unknown variable name")

let rec codegen_expr env = function
  | AST.Number n -> Llvm.const_float (double_type env) n
  | AST.Variable name -> lookup_var env.named_values name
  | AST.Binary (op, lhs, rhs) ->
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
  | AST.Call (callee, args) ->
      let callee =
        match Llvm.lookup_function callee env.the_module with
        | Some callee -> callee
        | None        -> raise (Error "unknown function referenced")
      in
      let params = Llvm.params callee in
      if Array.length params != Array.length args then raise (Error "incorrect # of arguments passed");
      let args = Array.map (codegen_expr env) args in
      Llvm.build_call callee args "calltmp" env.builder

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
