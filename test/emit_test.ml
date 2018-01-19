let target_triple_test () =
  Alcotest.(check (option string))
    "same string option"
    (Util.match_regexp_in_process_output
       "clang --version"
       (Str.regexp "Target: \\([^ ]*\\)"))
    (Some (Emit.get_target_triple ()))

let linking_test () =
  let obj_dir = "_build/test-data" in
  let obj     = obj_dir ^ "/average.o" in
  let env     = Codegen.create_env () in
  let _       = Sys.command ("mkdir -p " ^ obj_dir) in
  let ()      = Util.create_obj env "test-data/average.tv" obj () in
  let _       = Sys.command ("c++ examples/average_main.cpp " ^ obj ^ " -o average_main") in
  Alcotest.(check (option string))
    "same string option"
    (Some "3.5")
    (Util.match_regexp_in_process_output
       "./average_main"
       (Str.regexp "average of 3.0 and 4.0: \\([^ ]*\\)"))

let emit_set =
  [ "Get the target triple", `Quick, target_triple_test
  ; "Link an emission",      `Quick, linking_test
  ]
