Random.self_init ()

open Board
open Tile
module B = Board
module T = Tile

type game = {
  mutable time_elapsed : float;
  mutable score : int;
  mutable words_found : string list;
  mutable board : B.letter list list;
}

let init_game =
  { time_elapsed = 0.0; score = 0; words_found = []; board = B.empty }

(*let move (t : t) (start_pos : int * int) (end_pos : int * int) =*)

let rec row_to_string (row : char list) =
  match row with
  | [] -> " |"
  | h :: t -> " | " ^ String.make 1 h ^ row_to_string t

let rec string_of_board (game_board : char list list) =
  match game_board with
  | [] -> ""
  | h :: t -> row_to_string h ^ "\n" ^ string_of_board t

let get_string_of_board (game_board : char list list) =
  "___________________________________\n" ^ string_of_board game_board
  ^ "___________________________________\n"
