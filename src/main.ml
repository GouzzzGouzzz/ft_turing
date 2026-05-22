
let main () =
  if Parser.parse_option () = true then (
    exit 1
  )
  else (
    let filename = Sys.argv.(1) in
    let json = Yojson.Safe.from_file filename in
    let machine = Parser.create_machine json in
    Debug.print_machine machine
  ) 


let () = main ()