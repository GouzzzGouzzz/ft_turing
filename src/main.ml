let main () =
  if Parser.parse_option () = true then (
    exit 1
  )
  else (
    let filename = Sys.argv.(1) in
    try
      let json = Yojson.Safe.from_file filename in
      let machine = Parser.create_machine json in
      Parser.validate_fields machine;
      (*Debug.print_machine machine;*)
      let input = Parser.input Sys.argv.(2) machine.alphabet machine.blank in
      Print.print_init machine;
      Turing.simulate machine input;
      ()
    with
    | Sys_error msg  -> Print.print_error msg;
    | Yojson.Json_error msg-> Print.print_error msg;
    | Parser.Parse_error msg -> Print.print_error msg;
    | Parser.Validation_error msg -> Print.print_error msg;
    | Yojson.Safe.Util.Type_error (msg, json) -> Print.print_error ("(JSON FORMATING): " ^ msg);
    | Turing.Runtime_error msg -> Print.print_error msg;
  )

let () = main ()