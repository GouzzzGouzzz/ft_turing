

let create_tape str blank =
  let tape : Types.tape =
  {
    left = [blank];
    head = str.[0];
    right = List.init (String.length str - 1) (fun x -> str.[x + 1])
  } in tape

let simulate (machine : Types.machine) input =
  let tape = create_tape input machine.blank in
  Debug.print_tape tape;