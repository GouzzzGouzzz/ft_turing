
let main () =
  if Array.length Sys.argv < 2 then ( 
    Printf.eprintf "Usage: %s <file.json>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let json = Yojson.Safe.from_file filename in
  let machine = Parser.create_machine json in
  Debug.print_machine machine

    
let () = main ()