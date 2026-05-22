let delete_newline str =
  String.map (function '\n' -> ' ' | x -> x) str

let print_error str =
  Printf.printf "Ft_turing: Error: %s\n" (delete_newline str)


let main () =
  if Parser.parse_option () = true then (
    exit 1
  )
  else (
    let filename = Sys.argv.(1) in
    try
      let json = Yojson.Safe.from_file filename in
      let machine = Parser.create_machine json in
      Debug.print_machine machine
    with
    | Sys_error msg  -> print_error msg;
    | Yojson.Json_error msg-> print_error msg;
  ) 

let () = main ()