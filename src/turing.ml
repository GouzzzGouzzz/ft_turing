let rec remove_last_elt list = 
  match list with
  | [] -> []
  | hd :: [] -> [] (* if end of list do not return the element *)
  | hd :: tail -> hd :: (remove_last_elt tail)


let create_tape str blank =
  let tape : Types.tape =
  {
    left = [];
    head = str.[0];
    right = List.init (String.length str - 1) (fun x -> str.[x + 1]);
    index = 0
  } in tape

let move_right (tape : Types.tape) blank = 
  let new_tape : Types.tape =
  {
    head = List.nth tape.right 0;
    left = tape.left @ [tape.head];
    right = List.drop 1 tape.right;
    index = tape.index + 1
  } in new_tape

let move_left (tape : Types.tape) blank = 
  let new_tape : Types.tape =
  {
    head = List.nth tape.left ((List.length tape.left) - 1);
    left = remove_last_elt tape.left;
    right = tape.head :: tape.right;
    index = tape.index - 1
  } in new_tape


let simulate (machine : Types.machine) input =
  let tape = create_tape input machine.blank in
  let first_transition = List.hd (snd (List.hd machine.transitions)) in
    Print.print_current_tape_state "test" tape first_transition machine.blank;
  let tape = move_right tape machine.blank in 
    Print.print_current_tape_state "test" tape first_transition machine.blank;
  let tape = move_right tape machine.blank in 
    Print.print_current_tape_state "test" tape first_transition machine.blank;
  let tape = move_right tape machine.blank in 
    Print.print_current_tape_state "test" tape first_transition machine.blank;
  let tape = move_left tape machine.blank in 
    Print.print_current_tape_state "test" tape first_transition machine.blank;
  Debug.print_tape tape;