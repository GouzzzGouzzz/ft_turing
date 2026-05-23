let delete_newline str =
  String.map (function '\n' -> ' ' | x -> x) str

let print_error str =
  Printf.printf "Ft_turing: %s\n" (delete_newline str)