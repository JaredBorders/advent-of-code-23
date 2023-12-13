let split = function
  | s -> String.split_on_char ' ' s
;;

let solve_game_1 = function
  | line ->
    let split_line = split line in
    let id_and_colon = List.nth split_line 1 in
    let id = String.sub id_and_colon 0 (String.index id_and_colon ':') in
    let rec solve_colors amount = function
      | [] -> int_of_string id
      | h :: t ->
        (match int_of_string_opt h with
         | Some x -> solve_colors x t
         | None ->
           if h = "red" || h = "red," || h = "red;"
           then if amount <= 12 then solve_colors 0 t else 0
           else if h = "green" || h = "green," || h = "green;"
           then if amount <= 13 then solve_colors 0 t else 0
           else if h = "blue" || h = "blue," || h = "blue;"
           then if amount <= 14 then solve_colors 0 t else 0
           else solve_colors 0 t)
    in
    solve_colors 0 split_line
;;

let solve file =
  let channel = open_in file in
  let rec parse' channel sum =
    match input_line channel with
    | line -> parse' channel (solve_game_1 line + sum)
    | exception End_of_file -> print_int sum
  in
  parse' channel 0
;;

let () = solve "input.txt"
