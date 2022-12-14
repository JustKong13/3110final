Random.self_init ()

open Wordvalidator
open Board
open Tile
open String
module B = Board
open Tile
module T = Tile

exception TileNotFound
exception OutOfBound
exception TileOverlap

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

let place_adjacent (t1 : t) (end_pos : int * int) =
  match t1.ttype with
  | ATile -> new_coords t1 end_pos
  | HTile ->
      if fst end_pos - 1 > -1 then new_coords t1 (fst end_pos - 1, snd end_pos)
      else raise OutOfBound
  | VTile ->
      if snd end_pos - 1 > -1 then new_coords t1 (fst end_pos, snd end_pos - 1)
      else raise OutOfBound

let check_overlap (tpair : t * bool) (new_space : int * int)
    (pos_list : (int * int) list) =
  match snd tpair with
  | true ->
      if
        List.mem new_space pos_list
        || List.mem (adjacent (fst tpair) new_space) pos_list
      then false
      else true
  | false ->
      let new_tile = place_adjacent (fst tpair) new_space in
      if
        List.mem new_tile.position pos_list
        || List.mem (adjacent new_tile new_tile.position) pos_list
      then false
      else true

let check_bounds (tpair : t * bool) (new_space : int * int)
    (pos_list : (int * int) list) =
  match snd tpair with
  | true ->
      if fst new_space > max_x (fst tpair) || snd new_space > max_y (fst tpair)
      then false
      else true
  | false ->
      let new_tile = place_adjacent (fst tpair) new_space in
      if
        fst new_tile.position > max_x (fst tpair)
        || snd new_tile.position > max_y (fst tpair)
      then false
      else true

let rec find_tile (start_pos : int * int) (t_list : t list) =
  match t_list with
  | h :: t ->
      if h.position = start_pos then (h, true)
      else if adjacent h h.position = start_pos then (h, false)
      else find_tile start_pos t
  | [] -> raise TileNotFound

let move (start_pos : int * int) (end_pos : int * int) (t_list : t list) =
  let t1 = find_tile start_pos t_list in
  let rec move_aux t_list =
    match t_list with
    | [] -> []
    | h :: t ->
        if h.position = (fst t1).position && snd t1 = true then
          new_coords h end_pos :: move_aux t
        else if h.position = (fst t1).position && snd t1 = false then
          place_adjacent h end_pos :: move_aux t
        else h :: move_aux t
  in
  if check_overlap t1 end_pos (get_list_pos (fst t1) t_list) = false then
    raise TileOverlap
  else if check_bounds t1 end_pos (get_list_pos (fst t1) t_list) = false then
    raise OutOfBound
  else move_aux t_list

let move_on_board ((x1, y1) : int * int) ((x2, y2) : int * int)
    (board : B.letter list list) =
  if B.is_empty (x1, y1) board then raise TileNotFound
  else if B.is_empty (x2, y2) board then
    let removed_letter_board = B.remove_letter (x1, y1) board in
    B.place_letter (B.get_letter (x1, y1) board) (x2, y2) removed_letter_board
  else raise TileOverlap

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

let calc_score (h : string) =
  if length h = 2 then 0
  else if length h = 3 then 100
  else if length h = 4 then 400
  else if length h = 5 then 800
  else if length h = 6 then 1400
  else if length h = 7 then 1800
  else 2200

let rec update_score (word_list : string list) =
  match word_list with
  | [] -> 0
  | h :: t -> calc_score h + update_score t

let rec new_move (found : string list) (game_state : game) =
  match found with
  | [] -> ()
  | h :: t ->
      if List.mem h game_state.words_found || length h < 3 then
        new_move t game_state
      else game_state.words_found <- h :: game_state.words_found;
      game_state.score <- update_score game_state.words_found;
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
    columns and rows for words. Returns list of valid words*)
let rec check_for_words ((x, y) : int * int) (game_state : game) =
  (get_row y (B.board_to_list game_state.board)
  |> create_string_of_row |> generate_list_of_words |> get_valid_words)
  @ (get_col x (B.board_to_list game_state.board)
    |> create_string_of_row |> generate_list_of_words |> get_valid_words)