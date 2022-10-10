type game = {
  time_elapsed : float;
  score : int;
  words_found : string list;
}

let alphabet_double_vowels = "aabcdeefghiijklmnoopqrstuuvwxyz"

let available_strs_aux alphabet_string =
  let size = Random.int 2 in
  if size = 0 then String.make 1 (String.get alphabet_string (Random.int 32))
  else
    let first_letter =
      String.make 1 (String.get alphabet_string (Random.int 32))
    in
    let new_alphabet_string =
      Str.global_replace (Str.regexp first_letter) "" alphabet_string
    in
    let second_letter =
      String.make 1 (String.get new_alphabet_string (Random.int 32))
    in
    first_letter ^ second_letter

let rec available_strs alphabet_string accumulated_lst =
  if List.length accumulated_lst < 10 then
    let new_str = available_strs_aux alphabet_string in
    available_strs alphabet_string (new_str :: accumulated_lst)
  else accumulated_lst
