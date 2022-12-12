(* This file was referenced using FA22 CS 3110 A2 assignment. *)

open Game.Wordbite
open Game.Wordvalidator
open Game.Tile

exception InvalidString
exception InvalidCoord
exception TileNotFound

module G = Game.Wordbite

let game_state =
  {
    score = 0;
    words_found = [];
    board = generate_game_board tile_list game_board;
    tile_list = tiles;
  }

let rec start_game input =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n\nHere is your board\n";
  print_string (G.get_string_of_board (B.board_to_list game_state.board));
  print_endline ("\n" ^ lst_string game_state.tile_list);
  print_endline
    "\n\
     Move a character to an empty spot (x1, y1) to (x2, y2) by typing (x1 y1) \
     (x2 y2). When you are done, type 'quit'. To view which words you've \
     already found, type 'found'";
  print_string "> ";
  match read_line () with
  | input -> parse_input input

and parse_input input =
  match input with
  | "quit" -> exit 0
  | "found" ->
      ANSITerminal.print_string []
        ("Words Found: \n[\n " ^ get_words_found game_state.words_found ^ "\n]");
      start_game input
  | input ->
      parse_coord input;
      start_game input

and get_words_found (words_found : string list) =
  match words_found with
  | [] -> ""
  | h :: t -> h ^ "\n " ^ get_words_found t

and parse_coord s =
  try
    let new_tlist =
      G.move
        ( int_of_string (String.make 1 (String.get s 1)),
          int_of_string (String.make 1 (String.get s 3)) )
        ( int_of_string (String.make 1 (String.get s 7)),
          int_of_string (String.make 1 (String.get s 9)) )
        game_state.tile_list
    in
    game_state.tile_list <- new_tlist;
    game_state.board <- generate_game_board new_tlist B.empty;
    game_state.words_found <-
      check_for_words
        ( int_of_string (String.make 1 (String.get s 7)),
          int_of_string (String.make 1 (String.get s 9)) )
        game_state
      @ check_for_words
          ( int_of_string (String.make 1 (String.get s 7)),
            int_of_string (String.make 1 (String.get s 9)) + 1 )
          game_state
      @ check_for_words
          ( int_of_string (String.make 1 (String.get s 7)) + 1,
            int_of_string (String.make 1 (String.get s 9)) )
          game_state
      @ game_state.words_found
  with a -> (
    match a with
    | Invalid_argument _ ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Your input was invalid. Please follow the format: \n  ";
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "  (x1 y1) (x2 y2). Example: (1 2) (3 4)"
    | TileNotFound ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "We could not find your tile \n"
    | _ ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Your input was invalid. Please follow the format: \n  ";
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "  (x1 y1) (x2 y2). Example: (1 2) (3 4)")

(** [play_game input] starts the Wordbite game if [input] is "start". *)
let rec play_game input =
  try
    match input with
    | "start" -> start_game input
    | "quit" -> exit 0
    | _ -> raise InvalidString
  with InvalidString -> (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\nYou did not type in \'start\' correctly...\n";
    print_endline "Please type \'start\' to start the game.\n";
    print_string "> ";
    match read_line () with
    | input -> play_game input)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n\n\
     ╔══════════════════════════*.·:·.✧ ✦ ✧.·:·.*════════════════════════════╗\n\
    \                            Welcome to Wordbite!\n\n\
    \  How to play: \n\n\
    \  1. Create as many words as you can by moving tiles. \n\n\
    \  2. Letters may be a (1 x 1) tile or a pair aligned vertically (2 x 1) \n\
    \     or horizontally (1 x 2). \n\n\
    \  3. To move a tile, type in two coordinates:\n\
    \     1st coord: the tile you wish to move \n\
    \     2nd coord: position you wish to move it to. \n\
    \     To move tile pairs, use the leftmost or topmost coordinate. \n\
    \     Example: (1 2) (3 4) moves tile at (1, 2) to (3, 4)\n\n\
    \  4. You may only move tiles to empty spaces on the board. \n\
    \     They cannot go out of bounds. \n\n\
    \  5. Type 'quit' when done!\n\n\
     ╚══════════════════════════*.·:·.✧ ✦ ✧.·:·.*════════════════════════════╝\n\
    \ ";
  print_endline "Please type \'start\' to start the game.\n";
  print_string "> ";
  match read_line () with
  | input -> play_game input

(* Execute the game engine. *)
let () = main ()