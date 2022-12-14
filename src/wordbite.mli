open Tile
open Board
module T = Tile
module B = Board

exception TileNotFound
exception OutOfBound
exception TileOverlap

type game = {
  mutable score : int;
  mutable words_found : string list;
  mutable board : B.letter list list;
  mutable tile_list : T.t list;
}

(* [game] represents the game state. Score is currently how many points the
   player has in the game. Sore is calculated as follows: 3-letter words: 100
   points, 4-letter words: 400 points, 5-letter words: 800 points, 6-letter
   words: 1400, 7-letter words: 1800, anything else: 2200. Words found is a list
   of all the words the player made. The board is the visual representation of
   the tile list, which is the list of all the tiles (thier contents, alignment,
   and position. *)

val init_game : game
(* [init_game] is the game state at the very beginning of the game. The score is
   0, the words found list is empty, and the board is a 2D array repesentation
   of randomly generated tiles at random valid positions. *)

val tiles : T.t list
(* [tiles] is the the current list of tiles in the game.*)

val game_board : B.t
(* [game_board] is the 2D array representation of the current list of tiles in
   the game.*)

val move : int * int -> int * int -> T.t list -> T.t list
(* changes the list of tiles by changing the position of the tile specified by
   the first coordinate input, to a new position specified by the second
   coordinate input.*)

val new_move : string list -> game -> unit
(* [new_move found game_state] updates the game's words found list and score. *)

val generate_game_board : T.t list -> B.t -> B.t
(* generates a 2D array representation of the game's list of tiles on a 8 x 9
   grid. *)

val get_string_of_board : string list list -> string
(* generates a visual string representing the game board *)

val check_for_words : int * int -> game -> string list
(* [check_for_words (x, y) game_state] returns a list of strings of words found
   at row x and column y on the game board in game_state*)

(*Testing functions*)

val update_game_state : string -> game -> game
(* This method is here for testing purposes only*)

val move_on_board :
  int * int -> int * int -> letter list list -> letter list list
(* This method is here for testing purposes only*)

val create_string_of_row : string list -> string
(* This method is here for testing purposes only*)

val find_tile : int * int -> T.t list -> T.t * bool
(* This method is here for testing purposes only*)

val get_valid_words : string list -> string list
(* This method is here for testing purposes only*)

val generate_list_of_words : string -> string list
(* This method is here for testing purposes only*)