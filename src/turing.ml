let rec fill_blank list blank =
  if List.length list < 24 then
    fill_blank (list @ [blank]) blank
  else
    list

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
    right = fill_blank (List.init (String.length str - 1) (fun x -> str.[x + 1])) blank;
    index = 0;
    leftmost = 0;
    rightmost = 24
  } in tape

let move_right (tape : Types.tape) blank = 
  let new_tape : Types.tape =
  {
    head =  if (List.length tape.right) = 0 then blank else List.nth tape.right 0;
    left = tape.left @ [tape.head]; 
    right = List.drop 1 tape.right;
    index = tape.index + 1;
    leftmost = if tape.leftmost + 24 < tape.index + 1 then tape.leftmost + 1 else tape.leftmost;
    rightmost = if tape.rightmost < tape.index + 1 then tape.rightmost + 1 else tape.rightmost
  } in new_tape

let move_left (tape : Types.tape) blank = 
  let new_tape : Types.tape =
  {
    head = if (List.length tape.left) = 0 then blank else List.nth tape.left ((List.length tape.left) - 1);
    left = remove_last_elt tape.left;
    right = tape.head :: tape.right;
    index = tape.index - 1;
    leftmost = if tape.leftmost > tape.index - 1 then tape.leftmost - 1 else tape.leftmost;
    rightmost = if tape.rightmost + 24 > tape.index - 1 then tape.rightmost - 1 else tape.rightmost
  } in new_tape

let simulate (machine : Types.machine) input =
  let tape = create_tape input machine.blank in
  let first_transition = List.hd (snd (List.hd machine.transitions)) in
    Print.print_current_tape_state "test" tape first_transition machine.blank;
  Debug.print_tape tape;
  let tape = move_left tape machine.blank in 
    Print.print_current_tape_state "test" tape first_transition machine.blank;
  Debug.print_tape tape;
  let tape = move_left tape machine.blank in 
    Print.print_current_tape_state "test" tape first_transition machine.blank;
  Debug.print_tape tape;
  let tape = move_left tape machine.blank in 
    Print.print_current_tape_state "test" tape first_transition machine.blank;
  let tape = move_right tape machine.blank in 
    Print.print_current_tape_state "test" tape first_transition machine.blank;
  let tape = move_right tape machine.blank in 
    Print.print_current_tape_state "test" tape first_transition machine.blank;
  let tape = move_right tape machine.blank in 
    Print.print_current_tape_state "test" tape first_transition machine.blank;
  Debug.print_tape tape;