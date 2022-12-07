Random.self_init ()

open Board
module B = Board
open Tile
module T = Tile

type game = {
  mutable time_elapsed : float;
  mutable score : int;
  mutable words_found : string list;
  mutable board : B.letter list list;
  mutable tile_list : t list;
}

let tile_list = T.tile_list
let game_board = ref B.empty

let rec generate_game_board tile_list game_board =
  match tile_list with
  | [] -> game_board
  | h :: t -> begin
      match h.ttype with
      | ATile ->
          B.place_letter h.tstring h.position (generate_game_board t game_board)
      | HTile ->
          B.place_letter
            (String.make 1 (String.get h.tstring 0))
            h.position
            (B.place_letter
               (String.make 1 (String.get h.tstring 1))
               (fst h.position + 1, snd h.position)
               (generate_game_board t game_board))
      | VTile ->
          B.place_letter
            (String.make 1 (String.get h.tstring 0))
            h.position
            (B.place_letter
               (String.make 1 (String.get h.tstring 1))
               (fst h.position, snd h.position + 1)
               (generate_game_board t game_board))
    end

let init_game =
  {
    time_elapsed = 0.0;
    score = 0;
    words_found = [];
    board = generate_game_board tile_list !game_board;
    tile_list;
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

let rec row_to_string (row : string list) =
  match row with
  | [] -> " |"
  | h :: t -> " | " ^ h ^ row_to_string t

let rec string_of_board (game_board : string list list) (n : int) =
  match game_board with
  | [] -> ""
  | h :: t ->
      string_of_int n ^ row_to_string h ^ "\n" ^ string_of_board t (n + 1)

let get_string_of_board (game_board : string list list) =
  "    0   1   2   3   4   5   6   7   \n\
   ___________________________________  \n"
  ^ string_of_board game_board 0
  ^ "___________________________________\n"
