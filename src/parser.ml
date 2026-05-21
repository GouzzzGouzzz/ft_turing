
(* Peux crash si char == empty*)
let get_char_json jsonstr =
  jsonstr.[0]

let create_machine json = 
  let machine : Types.machine = 
    {
      name = json |> Yojson.Safe.Util.member "name" |> Yojson.Safe.Util.to_string;
      alphabet = json |> Yojson.Safe.Util.member "alphabet" |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string |> List.map get_char_json; 
      blank = json |> Yojson.Safe.Util.member "blank" |> Yojson.Safe.Util.to_string |> get_char_json; 
      states = json |> Yojson.Safe.Util.member "states" |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string;
      initial = json |> Yojson.Safe.Util.member "initial" |> Yojson.Safe.Util.to_string;
      finals = json |> Yojson.Safe.Util.member "finals" |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string;
    } in 
    machine