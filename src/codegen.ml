exception Error of string

let context    = Llvm.global_context ()
let the_module = Llvm.create_module context "top-level module"
let builder    = Llvm.builder context

let named_values : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10

let double_type = Llvm.double_type context

let lookup_var table name =
  try
    Hashtbl.find table name
  with Not_found ->
    raise (Error "unknown variable name")

let rec codegen_expr = function
  | AST.Number n -> Llvm.const_float double_type n
  | AST.Variable name -> lookup_var named_values name
  | AST.Binary (op, lhs, rhs) ->
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      begin
        match op with
        | '+' -> Llvm.build_fadd lhs_val rhs_val "addtmp" builder
        | '-' -> Llvm.build_fsub lhs_val rhs_val "subtmp" builder
        | '*' -> Llvm.build_fmul lhs_val rhs_val "multmp" builder
        | '<' ->
            (* Convert bool 0/1 to double 0.0 or 1.0 *)
            let i = Llvm.build_fcmp Llvm.Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
            Llvm.build_uitofp i double_type "booltmp" builder
        | _ -> raise (Error "invalid binary operator")
      end
  | AST.Call (callee, args) ->
      let callee =
        match Llvm.lookup_function callee the_module with
        | Some callee -> callee
        | None        -> raise (Error "unknown function referenced")
      in
      let params = Llvm.params callee in
      if Array.length params != Array.length args then raise (Error "incorrect # of arguments passed");
      let args = Array.map codegen_expr args in
      Llvm.build_call callee args "calltmp" builder

let codegen_proto = function
  | AST.Prototype (name, args) ->
      (* Make the function type: double(double, double) etc. *)
      let doubles = Array.make (Array.length args) double_type in
      let ft      = Llvm.function_type double_type doubles in
      let f =
        match Llvm.lookup_function name the_module with
        | None -> Llvm.declare_function name ft the_module
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
          Hashtbl.add named_values n a)
        (Llvm.params f);
      f

let codegen_func = function
  | AST.Function (proto, body) ->
      Hashtbl.clear named_values;
      let the_function = codegen_proto proto in
      let bb           = Llvm.append_block context "entry" the_function in
      Llvm.position_at_end bb builder;
      try
        let ret_val = codegen_expr body in
        ignore (Llvm.build_ret ret_val builder);
        Llvm_analysis.assert_valid_function the_function;
        the_function
      with e ->
        Llvm.delete_function the_function;
        raise e
