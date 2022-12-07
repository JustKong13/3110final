Random.self_init ()

open Board
open Tile
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

(*let move (t : t) (start_pos : int * int) (end_pos : int * int) =*)

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
