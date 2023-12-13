type game =
  { id : int
  ; red : int
  ; green : int
  ; blue : int
  }

let solve file =
  let channel = open_in file in
  let rec parse' channel =
    match input_line channel with
    | line ->
      let game_id = String.sub line 5 (String.index line ':' - 5) in
      let game = { id = int_of_string game_id; red = 0; green = 0; blue = 0 } in
      print_int (game.id + game.red + game.green + game.blue);
      parse' channel
    | exception End_of_file -> ()
  in
  parse' channel
;;

let () = solve "input.txt"
