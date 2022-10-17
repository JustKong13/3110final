type game = {
  time_elapsed : float;
  score : int;
  words_found : string list;
}

let alphabet_double_vowels = "aabcdeefghiijklmnoopqrstuuvwxyz"

let file_to_list filename =
  let ic = open_in filename and l = ref [] in
  let rec loop () =
    let line = input_line ic in
    l := line :: !l;
    loop ()
  in
  try loop ()
  with End_of_file ->
    close_in ic;
    List.rev !l

let banned = file_to_list "banned.txt"

let get_letter a_string =
  String.make 1 (String.get a_string (Random.int (String.length a_string)))

let rec available_strs_aux a_string =
  let size = Random.int 2 in
  if size = 0 then get_letter a_string
  else
    let first_letter = get_letter a_string in
    let new_alphabet_string =
      Str.global_replace (Str.regexp first_letter) "" a_string
    in
    let second_letter = get_letter new_alphabet_string in
    if List.mem (first_letter ^ second_letter) banned then
      available_strs_aux a_string
    else first_letter ^ second_letter

let rec available_strs a_string accumulated_lst =
  if List.length accumulated_lst < 10 then
    let new_str = available_strs_aux a_string in
    available_strs a_string (new_str :: accumulated_lst)
  else accumulated_lst
