exception Parse_error of string
exception Validation_error of string

let p_error = "Parse_error: "
let v_error = "Validation_error: "

(* Parsing with field requirments validation *)

let force_non_empty field str =
  if str = "" then
    raise (Parse_error (p_error ^ "Expected non empty field : " ^ field))
  else
    str

let get_char_json field str =
  if String.length str = 1 then
    str.[0]
  else
    raise (Parse_error (p_error ^ "Expected single character on field : " ^ field))

let get_action json =
  match json with 
  | "LEFT" -> Types.LEFT
  | "RIGHT" -> Types.RIGHT
  | _ -> raise (Parse_error (p_error ^ "Invalid action in transition, can only be RIGHT or LEFT"))

let get_transition json = 
  let transition : Types.transition =
    {
      read = json |> Yojson.Safe.Util.member "read" |> Yojson.Safe.Util.to_string |> get_char_json "read";
      to_state = json |> Yojson.Safe.Util.member "to_state" |> Yojson.Safe.Util.to_string |> force_non_empty "to_state";
      write = json |> Yojson.Safe.Util.member "write" |> Yojson.Safe.Util.to_string |> get_char_json "write";
      action = json |> Yojson.Safe.Util.member "action" |> Yojson.Safe.Util.to_string |> get_action;
    } in transition

let get_transitions (key, value) =
  let transition = 
    value
    |> Yojson.Safe.Util.to_list
    |> List.map get_transition
  in
  (key, transition)

let parse_transitions json =
  json |> Yojson.Safe.Util.to_assoc |> List.map get_transitions

let create_machine json = 
  let machine : Types.machine = 
    {
      name = json |> Yojson.Safe.Util.member "name" |> Yojson.Safe.Util.to_string;
      alphabet = json |> Yojson.Safe.Util.member "alphabet" |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string |> List.map (get_char_json "alphabet");
      blank = json |> Yojson.Safe.Util.member "blank" |> Yojson.Safe.Util.to_string |> get_char_json "blank"; 
      states = json |> Yojson.Safe.Util.member "states" |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string;
      initial = json |> Yojson.Safe.Util.member "initial" |> Yojson.Safe.Util.to_string |> force_non_empty "initial";
      finals = json |> Yojson.Safe.Util.member "finals" |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string;
      transitions = json |> Yojson.Safe.Util.member "transitions" |> parse_transitions
    } in machine

let parse_option () : bool =
  if Array.length Sys.argv = 1 then (
    Printf.printf "Usage: ft_turing <file.json> <input>\n";
    Printf.printf "Try 'ft_turing --help' for more information.\n";
    true
  )
  else if Array.length Sys.argv = 2 && (Sys.argv.(1) = "--help" || Sys.argv.(1) = "-h") then (
    Printf.printf "usage: ft_turing [-h] jsonfile input
    \npositional arguments:
      jsonfile        json description of the machine

      input           input of the machine
    \noptional arguments:
      -h, --help show this help message and exit\n";
    true
  )
  else if Array.length Sys.argv < 3 || Array.length Sys.argv > 3   then ( 
    Printf.printf "Usage: %s <file.json> <input>\n" Sys.argv.(0);
    true
  )
  else
    false


let input str alphabet blank =
  if String.length str = 0 
    then raise (Parse_error (p_error ^ "Empty input")); 
  String.iter (fun x -> 
    if not (List.mem x alphabet) || x = blank 
      then raise (Parse_error (p_error ^ "Invalid input, only alphabet char are allowed / blank are not allowed"))) str;
  str

(* Data validation of fields parsed *)

let validate_alphabet blank alphabet transitions =
  if not (List.mem blank alphabet) then raise (Validation_error (v_error ^ "has to be defined in alphabet"));
  let transition_data = List.map Pair.snd transitions in 
  let transition_state = List.flatten transition_data in
  let unique_list = List.sort_uniq Char.compare alphabet in
  if not (List.for_all (fun (x : Types.transition) -> List.mem x.read alphabet ) transition_state) 
    then raise (Validation_error (v_error ^ "Invalid char in field read of a transition"));
  if not (List.for_all (fun (x : Types.transition) -> List.mem x.write alphabet ) transition_state) 
    then raise (Validation_error (v_error ^ "Invalid char in field write of a transition"));
  if (List.length unique_list) != (List.length alphabet)
    then raise (Validation_error (v_error ^ "Invalid alphabet, no duplicate allowed"))

let validate_initial initial states transitions =
  if not (List.mem initial states) 
    then raise (Validation_error (v_error ^ "Inital state not defined in states list"));
  let transition_name = List.map Pair.fst transitions in 
  if not (List.mem initial transition_name)
    then raise (Validation_error (v_error ^ "Initial state is not defined in the transitions list"))


let read_check (elt : string * Types.transition list) =
  let transi_list = Pair.snd elt in
  let read_list = List.map (fun (x : Types.transition) -> x.read) transi_list in
  let read_uniq = List.sort_uniq Char.compare read_list in
  if (List.length read_uniq) != (List.length read_list)
    then raise (Validation_error (v_error ^ "Multiple states for the same read value"))

let validate_transitions states transitions =
  if List.length states = 0 then raise (Validation_error (v_error ^ "states need to be defined"));
  let transition_name = List.map Pair.fst transitions in 
  let transition_name_unique = List.sort_uniq String.compare transition_name in
  let transition_data = List.map Pair.snd transitions in 
  let transition_state = List.flatten transition_data in
  let transition_state_name_unique = List.sort_uniq String.compare states in
  if not (List.for_all (fun (x : Types.transition) -> List.mem x.to_state states ) transition_state) 
    then raise (Validation_error (v_error ^ "Transition defined with a name not defined in states list"));
  if List.length transition_name < List.length states - 1
    then raise (Validation_error (v_error ^ "Some transition states are not defined in transition list"));
  if not (List.for_all (fun (x : string) -> List.mem x states ) transition_name)
    then raise (Validation_error (v_error ^ "Transition not defined in transition list"));
  if (List.length transition_state_name_unique) != (List.length states)
    then raise (Validation_error (v_error ^ "Invalid states list definition, no duplicate allowed"));
  if (List.length transition_name_unique) != (List.length transition_name)
    then raise (Validation_error (v_error ^ "Invalid states list, no duplicate allowed"));
  List.iter read_check transitions

let validate_finals states finals =
  let finals_unique = List.sort_uniq String.compare finals in 
  if List.length finals = 0 then 
    raise (Validation_error (v_error ^ "Finals states need to be defined"));
  if not (List.for_all (fun x -> List.mem x states) finals) 
    then raise (Validation_error (v_error ^ "Finals states is not defined in states list"));
  if (List.length finals_unique) != (List.length finals)
    then raise (Validation_error (v_error ^ "Invalid finals, no duplicate allowed"))
  
let validate_name name = 
  if String.length name > 60 
    then raise (Validation_error (v_error ^ "Name is too loog (max 60)"))

let validate_fields (machine : Types.machine) =
  validate_name machine.name;
  validate_alphabet machine.blank machine.alphabet machine.transitions;
  validate_initial machine.initial machine.states machine.transitions;
  validate_transitions machine.states machine.transitions;
  validate_finals machine.states machine.finals;
  ()


