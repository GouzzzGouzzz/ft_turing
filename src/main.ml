
(*
let read_file filename -> let func_name func_args
...
;
return value
*)

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content


let test = 
  let right = Types.RIGHT in

  (* print variant safely *)
  match right with
  | Types.LEFT -> Printf.printf "LEFT\n"
  | Types.RIGHT -> Printf.printf "RIGHT\n"


let main () =
  if Array.length Sys.argv < 2 then ( 
    Printf.eprintf "Usage: %s <file.json>\n" Sys.argv.(0);
    exit 1
  );
  let content = read_file Sys.argv.(1) in 
  Printf.printf "File loaded:\n%s\n" content
  let x = test 


let () = main ()