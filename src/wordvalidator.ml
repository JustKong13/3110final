<<<<<<< HEAD
(** This file only has one method available to use, [check_word] which checks if
    your word is a valid word*)

let valid_words : string list =
  let ic = open_in "./src/words.txt" in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  loop []

let rec is_valid_word word_list word =
  match word_list with
  | [] -> false
  | h :: _ when h = word -> true
  | _ :: t -> is_valid_word t word

let check_word word = is_valid_word valid_words (word ^ "\r")
=======
(** This file only has one method available to use, [check word], which checks
    if your word is a valid word*)

let valid_words =
  let ic = open_in "./src/words.txt" in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  loop []

let rec is_valid_word word_list word =
  match word_list with
  | [] -> false
  | h :: _ when h = word -> true
  | _ :: t -> is_valid_word t word

let check_word word = is_valid_word valid_words word
>>>>>>> c9790fde29c9a2a081376f7005d9d454d4469ec4
