
(*
let _test = 
  match Types.RIGHT with
  | Types.LEFT -> Printf.printf "LEFT\n"
  | Types.RIGHT -> Printf.printf "RIGHT\n"
*)

let main () =
  if Array.length Sys.argv < 2 then ( 
    Printf.eprintf "Usage: %s <file.json>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let json = Yojson.Safe.from_file filename in
  let machine = Parser.create_machine json in

  Printf.printf "Name: %s\n" machine.name;
  Printf.printf "Alphabet: ";
  List.iter (Printf.printf "%c ") machine.alphabet;
  Printf.printf "\n";
  Printf.printf "Blank: %c\n" machine.blank;
  Printf.printf "States: ";
  List.iter (Printf.printf "%s ") machine.states;
  Printf.printf "\n";
  Printf.printf "Initial: %s\n" machine.initial;
  Printf.printf "Finals: ";
  List.iter (Printf.printf "%s ") machine.finals;
  Printf.printf "\n"

let () = main ()