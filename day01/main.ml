let string_to_char_list s = List.of_seq (String.to_seq s)

let replace_word_integers line =
  let rec f' acc = function
    | [] -> acc
    | 'o' :: 'n' :: 'e' :: xs -> f' ('1' :: acc) ('n' :: 'e' :: xs)
    | 't' :: 'w' :: 'o' :: xs -> f' ('2' :: acc) ('w' :: 'o' :: xs)
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: xs -> f' ('3' :: acc) ('h' :: 'r' :: 'e' :: 'e' :: xs)
    | 'f' :: 'o' :: 'u' :: 'r' :: xs -> f' ('4' :: acc) ('o' :: 'u' :: 'r' :: xs)
    | 'f' :: 'i' :: 'v' :: 'e' :: xs -> f' ('5' :: acc) ('i' :: 'v' :: 'e' :: xs)
    | 's' :: 'i' :: 'x' :: xs -> f' ('6' :: acc) ('i' :: 'x' :: xs)
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: xs -> f' ('7' :: acc) ('e' :: 'v' :: 'e' :: 'n' :: xs)
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: xs -> f' ('8' :: acc) ('i' :: 'g' :: 'h' :: 't' :: xs)
    | 'n' :: 'i' :: 'n' :: 'e' :: xs -> f' ('9' :: acc) ('i' :: 'n' :: 'e' :: xs)
    | x :: xs -> f' (x :: acc) xs
  in
  List.rev @@ f' [] line
;;

let parse_char_array line =
  let is_digit c = c >= '0' && c <= '9' in
  let rec f' acc = function
    | [] -> acc
    | x :: xs ->
      if is_digit x then f' ((int_of_char x - int_of_char '0') :: acc) xs else f' acc xs
  in
  List.rev @@ f' [] line
;;

let parse_file filename =
  let in_channel = open_in filename in
  let sum = ref 0 in
  try
    while true do
      let line = input_line in_channel in
      let numbers =
        line |> string_to_char_list |> replace_word_integers |> parse_char_array
      in
      let length = List.length numbers in
      let line_sum =
        if length > 1
        then (List.hd numbers * 10) + List.nth numbers (length - 1)
        else (List.hd numbers * 10) + List.hd numbers
      in
      sum := !sum + line_sum
    done
  with
  | End_of_file ->
    close_in in_channel;
    print_endline (string_of_int !sum)
;;

let () = parse_file "input.txt"
