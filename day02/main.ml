let is_color color = function
  | c when c = color || c = color ^ "," || c = color ^ ";" -> true
  | _ -> false
;;

let split s = String.split_on_char ' ' s

let find_game_id split_line =
  let id_and_colon = List.nth split_line 1 in
  String.sub id_and_colon 0 (String.index id_and_colon ':')
;;

let solve_game_1 line =
  let split_line = split line in
  let id = find_game_id split_line in
  let rec solve_colors amount = function
    | [] -> int_of_string id
    | h :: t ->
      (match int_of_string_opt h with
       | Some x -> solve_colors x t
       | None when is_color "red" h && amount > 12 -> 0
       | None when is_color "green" h && amount > 13 -> 0
       | None when is_color "blue" h && amount > 14 -> 0
       | None -> solve_colors 0 t)
  in
  solve_colors 0 split_line
;;

let solve_game_2 line =
  let split_line = split line in
  let rec solve_colors curr r g b = function
    | [] -> r * g * b
    | h :: t ->
      (match int_of_string_opt h with
       | Some x -> solve_colors x r g b t
       | None when is_color "red" h && curr > r -> solve_colors 0 curr g b t
       | None when is_color "green" h && curr > g -> solve_colors 0 r curr b t
       | None when is_color "blue" h && curr > b -> solve_colors 0 r g curr t
       | None -> solve_colors 0 r g b t)
  in
  solve_colors 0 0 0 0 split_line
;;

let solve f file =
  let channel = open_in file in
  let rec parse sum =
    match input_line channel with
    | line -> parse (f line + sum)
    | exception End_of_file ->
      close_in channel;
      print_int sum
  in
  parse 0
;;

let () =
  solve solve_game_1 "input.txt";
  print_endline "";
  solve solve_game_2 "input.txt";
  print_endline ""
;;
