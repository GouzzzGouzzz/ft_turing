let rec fill_blank list blank =
  if List.length list < 24 then
    fill_blank (list @ [blank]) blank
  else
    list

let rec remove_last_elt list = 
  match list with
  | [] -> []
  | hd :: [] -> []
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

let rec get_transi_list (transitions : ( string * Types.transition list) list) curr_state = 
  match transitions with
  | [] -> None 
  | (str, lst) :: tl -> 
    if str = curr_state then Some lst else get_transi_list tl curr_state

let rec get_transi transi_list head =
  match transi_list with
  | [] -> None
  | (hd : Types.transition) :: tail -> 
    if hd.read = head then Some hd else get_transi tail head

let rec simulate_rec (machine : Types.machine) (tape : Types.tape) curr_state =
  let transi_list = get_transi_list machine.transitions curr_state in
  let transi = get_transi (Option.get transi_list) tape.head in
  let transi = (Option.get transi) in
    if (List.mem transi.to_state machine.finals)
      then Printf.printf "Fini"
    else
      Print.print_current_tape_state curr_state tape transi machine.blank;
      simulate_rec machine tape transi.to_state;
      ()

let simulate (machine : Types.machine) input =
  let init_tape = create_tape input machine.blank in
    simulate_rec machine init_tape machine.initial;
    ()
