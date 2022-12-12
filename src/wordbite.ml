Random.self_init ()

open Wordvalidator
open Board
open Tile
open String
module B = Board
open Tile
module T = Tile

exception TileNotFound

type game = {
  mutable score : int;
  mutable words_found : string list;
  mutable board : B.letter list list;
  mutable tile_list : t list;
}

let tiles = T.tile_list
let game_board = B.empty

let rec generate_game_board tiles game_board =
  match tiles with
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
    score = 0;
    words_found = [];
    board = generate_game_board tile_list game_board;
    tile_list = tiles;
  }

let rec get_list_pos (t1 : t) (t_list : t list) =
  match t_list with
  | [] -> []
  | h :: t ->
      if h = t1 then get_list_pos t1 t
      else h.position :: adjacent h h.position :: get_list_pos t1 t

let check_avail (t : t) (new_space : int * int) (pos_list : (int * int) list) =
  if
    List.mem new_space pos_list
    || List.mem (adjacent t new_space) pos_list
    || fst new_space > max_x t
    || snd new_space > max_y t
  then false
  else true

let rec find_tile (start_pos : int * int) (t_list : t list) =
  match t_list with
  | h :: t -> if h.position = start_pos then h else find_tile start_pos t
  | [] -> raise TileNotFound

let move (start_pos : int * int) (end_pos : int * int) (t_list : t list) =
  let t1 = find_tile start_pos t_list in
  let rec move_aux t_list =
    match t_list with
    | [] -> []
    | h :: t ->
        if h.position = t1.position then new_coords h end_pos :: move_aux t
        else h :: move_aux t
  in
  if check_avail t1 end_pos (get_list_pos t1 t_list) then move_aux t_list
  else failwith "Cannot move this tile due to overlapping."

let move_on_board ((x1, y1) : B.coord) ((x2, y2) : B.coord)
    (board : B.letter list list) =
  if B.is_empty (x1, y1) board then raise (Failure "Letter not found")
  else if B.is_empty (x2, y2) board then
    let removed_letter_board = B.remove_letter (x1, y1) board in
    B.place_letter (B.get_letter (x1, y1) board) (x2, y2) removed_letter_board
  else raise (Failure "Position is occupied")

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

(** [update_game_state] updates the game state on successful completion of a
    word on the board - this method is here for testing purposes only*)
let update_game_state (word : string) (game_state : game) =
  if List.mem word game_state.words_found then game_state
  else
    {
      score = game_state.score + 1;
      words_found = word :: game_state.words_found;
      board = game_state.board;
      tile_list = game_state.tile_list;
    }

let rec update_score (old_score : int) (found : string list) =
  match found with
  | [] -> old_score
  | h :: t ->
      if length h = 2 then update_score (old_score + 0) t
      else if length h = 3 then update_score (old_score + 100) t
      else if length h = 4 then update_score (old_score + 400) t
      else if length h = 5 then update_score (old_score + 800) t
      else if length h = 6 then update_score (old_score + 1400) t
      else if length h = 7 then update_score (old_score + 1800) t
      else update_score (old_score + 2200) t

let rec new_move (found : string list) (game_state : game) =
  match found with
  | [] -> ()
  | h :: t ->
      if List.mem h game_state.words_found || length h < 3 then
        new_move t game_state
      else game_state.words_found <- h :: game_state.words_found;
      game_state.score <- update_score game_state.score found;
      new_move t game_state

let rec get_valid_words (word_list : string list) =
  match word_list with
  | [] -> []
  | h :: t -> if check_word h then h :: get_valid_words t else get_valid_words t

let rec generate_list_of_words (row_in_string : string) =
  row_in_string |> Str.split (Str.regexp "-")

let rec create_string_of_row (row : string list) =
  match row with
  | [] -> ""
  | h :: t -> h ^ create_string_of_row t

let rec get_row (y : int) (board : string list list) =
  match board with
  | [] -> []
  | h :: t -> if y = 0 then h else get_row (y - 1) t

let rec get_col (x : int) (board : string list list) =
  match board with
  | [] -> []
  | h :: t -> List.nth h x :: get_col x t

(** [check_for_words] takes in [coords : int * int] and checks the respective
    columns and rows for words. Updates the game state if the word is found*)
let rec check_for_words ((x, y) : int * int) (game_state : game) =
  (get_row y (B.board_to_list game_state.board)
  |> create_string_of_row |> generate_list_of_words |> get_valid_words)
  @ (get_col x (B.board_to_list game_state.board)
    |> create_string_of_row |> generate_list_of_words |> get_valid_words)
