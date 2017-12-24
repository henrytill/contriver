exception Error of string

let get_target_triple =
  Llvm_target.Target.default_triple

let emit_module mdle file =
  let open Llvm_target in
  (* 8.2 Choosing a target *)
  Llvm_all_backends.initialize ();
  let triple = get_target_triple () in
  let targ   = Target.by_triple triple in
  if Target.has_target_machine targ then
    (* 8.3 Target Machine *)
    let machine = TargetMachine.create ~triple:triple targ in
    (* 8.4 Configuring the Module *)
    let layout  = TargetMachine.data_layout machine in
    Llvm.set_data_layout (DataLayout.as_string layout) mdle;
    Llvm.set_target_triple triple mdle;
    (* 8.5 Emit Object Code *)
    TargetMachine.emit_to_file mdle CodeGenFileType.ObjectFile file machine
  else
    raise (Error "target does not have an associated target machine")
