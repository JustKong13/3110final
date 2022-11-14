Random.self_init ()

type game = {
  time_elapsed : float;
  score : int;
  words_found : string list;
}

let banned =
  let ic = open_in "./src/banned.txt" in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  loop []

let get_letter a_string =
  String.make 1 (String.get a_string (Random.int (String.length a_string)))

let rec strings_aux a_string =
  let size = Random.int 2 in
  if size = 0 then get_letter a_string
  else
    let first = get_letter a_string in
    let new_alphabet_string =
      Str.global_replace (Str.regexp first) "" a_string
    in
    let second_letter = get_letter new_alphabet_string in
    if List.mem (first ^ second_letter) banned then strings_aux a_string
    else first ^ second_letter

let rec strings a_string acc =
  if List.length acc < 10 then
    let new_str = strings_aux a_string in
    strings a_string (new_str :: acc)
  else acc
