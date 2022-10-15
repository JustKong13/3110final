type game = {
  time_elapsed : float;
  score : int;
  words_found : string list;
}

let alphabet_double_vowels = "aabcdeefghiijklmnoopqrstuuvwxyz"

let get_letter a_string =
  String.make 1 (String.get a_string (Random.int (String.length a_string)))

let available_strs_aux a_string =
  let size = Random.int 2 in
  if size = 0 then get_letter a_string
  else
    let first_letter = get_letter a_string in
    let new_alphabet_string =
      Str.global_replace (Str.regexp first_letter) "" a_string
    in
    let second_letter = get_letter new_alphabet_string in
    first_letter ^ second_letter

let rec available_strs alphabet_string accumulated_lst =
  if List.length accumulated_lst < 10 then
    let new_str = available_strs_aux alphabet_string in
    available_strs alphabet_string (new_str :: accumulated_lst)
  else accumulated_lst
