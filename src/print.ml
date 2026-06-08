let delete_newline str =
  String.map (function '\n' -> ' ' | x -> x) str

let print_error str =
  Printf.printf "Ft_turing: %s\n" (delete_newline str)


let print_name name =
  let str_star = "********************************************************************************" in
  let name_len = String.length name in 
  let len = (String.length str_star) - name_len - 2 in
  (* -2 for left padding *)
  let left = (len / 2) - 2 in
  (* (-2 * 3) for the left padding, again 2 for center padding and again to be left centered*)
  let right = len - left - 6 in
  let left_str = String.make left ' ' in
  let right_str = String.make right ' ' in
  
  Printf.printf "%s\n" str_star;
  Printf.printf " *                                                                          * \n";
  Printf.printf "  *%s%s%s*\n" left_str name right_str;
  Printf.printf " *                                                                          * \n";
  Printf.printf "%s\n" str_star


let print_transition state (transition : Types.transition) = 
    Printf.printf "(%s, %c)" state transition.read;
    let action = if transition.action = Types.RIGHT then "RIGHT" else "LEFT" in
    Printf.printf " -> (%s, %c, %s)\n" transition.to_state transition.write action


let rec print_state (transitions : ( string * Types.transition list) list) =
  match transitions with
  | (state, transi) :: tail ->
      List.iter (print_transition state) transi;
      print_state tail
  | [] ->
      Printf.printf "********************************************************************************\n"


let print_machine (machine : Types.machine) =
  Printf.printf "Alphabet: ";
  Printf.printf "[ ";
  List.iter (Printf.printf "%c ") machine.alphabet;
  Printf.printf "]";


  Printf.printf "\nStates: ";
  Printf.printf "[ ";
  List.iter (Printf.printf "%s ") machine.states;
  Printf.printf "]";
  Printf.printf "\n";

  Printf.printf "Initial: %s\n" machine.initial;

  Printf.printf "Finals: ";
  Printf.printf "[ ";
  List.iter (Printf.printf "%s ") machine.finals;
  Printf.printf "]";
  Printf.printf "\n";

  print_state machine.transitions


let rec print_list charlist lenght =
  match charlist, lenght with
  | [], _ -> ()
  | _, 0 -> ()
  | c :: tail, _ -> Printf.printf "%c" c;
    print_list tail (lenght - 1)

  (*Total lenght is 25, we keep 12 for each left and right part of the tape, rest is head*)
let print_tape_status (tape : Types.tape) (transition : Types.transition) blank =
  let max_len = 12 in
  let left_list_len = if List.length tape.left > max_len then max_len else List.length tape.left in
  let right_list_len = if List.length tape.right > max_len then max_len else List.length tape.right in
  let left_pad = max_len - left_list_len  in
  let right_pad = max_len - right_list_len in
    Printf.printf "[";
    print_list tape.left left_list_len;
    Printf.printf "%s" (String.make left_pad blank);

    Printf.printf "<%c>" tape.head;

    print_list tape.right right_list_len;
    Printf.printf "%s" (String.make right_pad blank);
    Printf.printf "]"

let print_current_tape_state state (tape : Types.tape) (transition : Types.transition) blank = 
  print_tape_status tape transition blank;
  print_transition state transition

let print_init (machine : Types.machine) =
  print_name machine.name;
  print_machine machine