
(* Peux crash si char == empty*)
let get_char_json jsonstr =
  jsonstr.[0]

let get_action json =
  match json with 
  | "LEFT" -> Types.LEFT
  | "RIGHT" -> Types.RIGHT
  | _ -> failwith "invalid action in transtion, can only be RIGHT or LEFT"

let get_each_transition json = 
  let transition : Types.transition =
    {
      read = json |> Yojson.Safe.Util.member "read" |> Yojson.Safe.Util.to_string |> get_char_json;
      to_state = json |> Yojson.Safe.Util.member "to_state" |> Yojson.Safe.Util.to_string;
      write = json |> Yojson.Safe.Util.member "write" |> Yojson.Safe.Util.to_string |> get_char_json;
      action = json |> Yojson.Safe.Util.to_string |> get_action;
    } in transition


let get_transitions json =
  json |> Yojson.Safe.Util.to_assoc |> List.map get_each_transition

let create_machine json = 
  let machine : Types.machine = 
    {
      name = json |> Yojson.Safe.Util.member "name" |> Yojson.Safe.Util.to_string;
      alphabet = json |> Yojson.Safe.Util.member "alphabet" |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string |> List.map get_char_json; 
      blank = json |> Yojson.Safe.Util.member "blank" |> Yojson.Safe.Util.to_string |> get_char_json; 
      states = json |> Yojson.Safe.Util.member "states" |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string;
      initial = json |> Yojson.Safe.Util.member "initial" |> Yojson.Safe.Util.to_string;
      finals = json |> Yojson.Safe.Util.member "finals" |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string;
      transitions = json |> Yojson.Safe.Util.member "transitions" |> List.map get_transitions
    } in  machine