let solve file =
  let channel = open_in file in
  let rec parse acc =
    match input_line channel with
    | line ->
      print_endline line;
      parse @@ (acc + 1)
    | exception End_of_file -> close_in channel
  in
  parse 0
;;

let () =
  solve "input.txt";
  print_endline ""
;;
