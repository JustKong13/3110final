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

let rec get_list_pos (t_list : t list) =
  match t_list with
  | [] -> []
  | h :: t -> h.position :: adjacent h h.position :: get_list_pos t

let check_avail (t : t) (new_space : int * int) (pos_list : (int * int) list) =
  if List.mem new_space pos_list || List.mem (adjacent t t.position) pos_list
  then false
  else true

let move (t1 : t) (end_pos : int * int) (t_list : t list) =
  let rec move_aux (t_list : t list) =
    match t_list with
    | [] -> []
    | h :: t ->
        if h.position = t1.position then new_coords h end_pos :: move_aux t
        else h :: move_aux t
  in
  if check_avail t1 end_pos (get_list_pos t_list) then move_aux t_list
  else failwith "Cannot move this tile due to overlapping."

(*let check_for_word (t : t) (game_board : B.letter list list) = let board =
  B.board_to_list game_board in let row = fst (t.position) in let col = snd
  (t.position) in*)

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
