
let print_action (action : Types.action) =
  Printf.printf "  Action: ";
  match action with 
  | Types.LEFT -> Printf.printf " LEFT\n"
  | Types.RIGHT -> Printf.printf "RIGHT\n"


let print_transition (transi : Types.transition) = 
  Printf.printf "---\n";
  Printf.printf "  Read: %c\n" transi.read;
  Printf.printf "  To_state: %s\n" transi.to_state;
  Printf.printf "  Write: %c\n" transi.write;
  print_action transi.action

let rec print_state (transitions : ( _ * Types.transition list) list) =
  match transitions with
  | (state, transi) :: tail ->
      Printf.printf "---------\n";
      Printf.printf "State: %s\n" state;
      Printf.printf "Transtions:\n";
      List.iter print_transition transi;
      print_state tail
  | [] ->
      Printf.printf "---------\nEnd of transitions\n"


let print_machine (machine : Types.machine) =
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
  Printf.printf "\n";
  Printf.printf "Transitions:\n";
  print_state machine.transitions
