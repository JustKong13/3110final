(** Interface for wordbite module

    This module is reponsible for the game logic and uses board and tile module
    to advance the game. *)

open Tile
open Board

module T = Tile
(** Tile module. See Tile*)

module B = Board
(** Board module. See Board*)

exception TileNotFound
(** Exception for an empty tile selection. *)

exception OutOfBound
(** Exception for moving a tile out of bounds. *)

exception TileOverlap
(** Exception for moving a tile onto an already existing tile. *)

type game = {
  mutable score : int;
  mutable words_found : string list;
  mutable board : B.letter list list;
  mutable tile_list : T.t list;
}
(** [game] represents the game state. Score is currently how many points the
    player has in the game. Sore is calculated as follows: 3-letter words: 100
    points, 4-letter words: 400 points, 5-letter words: 800 points, 6-letter
    words: 1400, 7-letter words: 1800, anything else: 2200. Words found is a
    list of all the words the player made. The board is the visual
    representation of the tile list, which is the list of all the tiles (thier
    contents, alignment, and position. *)

val init_game : game
(** [init_game] is the game state at the very beginning of the game. The score
    is 0, the words found list is empty, and the board is a 2D array
    repesentation of randomly generated tiles at random valid positions. *)

val tiles : T.t list
(** [tiles] is the the current list of tiles in the game.*)

val game_board : B.t
(** [game_board] is the 2D array representation of the current list of tiles in
    the game.*)

val move : int * int -> int * int -> T.t list -> T.t list
(** changes the list of tiles by changing the position of the tile specified by
    the first coordinate input, to a new position specified by the second
    coordinate input.*)

val new_move : string list -> game -> unit
(** [new_move found game_state] updates the game's words found list and score. *)

val generate_game_board : T.t list -> B.t -> B.t
(** generates a 2D array representation of the game's list of tiles on a 8 x 9
    grid. *)

val get_string_of_board : string list list -> string
(** generates a visual string representing the game board *)

val check_for_words : int * int -> game -> string list
(** [check_for_words (x, y) game_state] returns a list of strings of words found
    at row x and column y on the game board in game_state*)

(*Testing functions*)

val update_game_state : string -> game -> game
(** Returns the game state given an original game state and a new word. If the
    word has already been found, returns original game state. *)

val update_score : string list -> int
(** Returns a score given a list of found words*)

val move_on_board :
  int * int -> int * int -> letter list list -> letter list list
(** Moves a letter from one tile to another tile*)

val create_string_of_row : string list -> string
(** Takes a row in the board and parses out words. Returns a list of words.*)

val find_tile : int * int -> T.t list -> T.t * bool
(** Returns the tile at a given position.*)

val get_valid_words : string list -> string list
(** Returns a list of valid words*)

val generate_list_of_words : string -> string list
(** Takes a string that represents a row or column of the board, and returns a
    list of words to be validated. *)
