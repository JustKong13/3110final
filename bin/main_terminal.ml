(* This file was referenced using FA22 CS 3110 A2 assignment. *)

open Game.Wordbite
open Game.Wordvalidator
open Game.Tile

exception InvalidString
exception InvalidCoord

module G = Game.Wordbite

let game_state =
  {
    time_elapsed = 0.0;
    score = 0;
    words_found = [];
    board = generate_game_board tile_list game_board;
    tile_list = tiles;
  }

let rec parse_coord s =
  let new_tlist =
    G.move
      ( int_of_string (String.make 1 (String.get s 1)),
        int_of_string (String.make 1 (String.get s 3)) )
      ( int_of_string (String.make 1 (String.get s 7)),
        int_of_string (String.make 1 (String.get s 9)) )
      game_state.tile_list
  in
  game_state.tile_list <- new_tlist;
  game_state.board <- generate_game_board new_tlist game_state.board

(** [parse_input s words] parses [s] checking if it is made up of strings from
    [words] and checks whether [s] is a real word, otherwise prompts the user
    for another input. *)

(* 3. Parse the string -> if string consists of strings from list, then check if
   it is a word *)
(* 4. Prints error message if string is considered Illegal *)
(* 5. Add string to list of checked words *)
(* 6. Repeat from step 2 *)

let rec move_tiles input =
  match input with
  | "quit" -> exit 0
  | input -> parse_coord input

(* | _ -> raise InvalidCoord with InvalidCoord -> ( ANSITerminal.print_string [
   ANSITerminal.red ] "\nThis input is not valid"; print_endline "\n\ Move a
   character to an empty spot (x1, y1) to (x2, y2) by typing (x1 \ y1) (x2 y2).
   When you are done, type 'quit'. "; print_string "> "; match read_line () with
   | input -> parse_coord input; move_tiles input) *)

(** [play_game input] starts the Wordbite game if [input] is "start". *)
let rec play_game input =
  try
    match input with
    | "start" -> (
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\n\nHere is your starting board\n";
        print_string (G.get_string_of_board (B.board_to_list game_state.board));
        print_endline
          "\n\
           Move a character to an empty spot (x1, y1) to (x2, y2) by typing \
           (x1 y1) (x2 y2). When you are done, type 'quit'. ";
        print_string "> ";
        print_endline ("\n" ^ lst_string game_state.tile_list);
        match read_line () with
        | input -> move_tiles input)
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
     Welcome to Wordbite.\n\n\
    \  How to play: \n\n\
    \  1. Create as many words as you can by moving tiles. \n\n\
    \  2. Letters may be a (1x1) tile or a pair aligned vertically (1x2) or \
     horizontally (2x1). \n\n\
    \  3. To move a tile, type in two coordinates: the tile you wish to move, \
     and where you wish to move it. To move tile pairs, use the leftmost or \
     topmost coordinate. Example: (1 2) (3 4) \n\n\
    \  4. You may only move tiles to empty spaces on the board and they cannot \
     go out of bounds. \n\n\
    \  5. Type 'quit' when done!\n\n\
    \  ";
  print_endline "Please type \'start\' to start the game.\n";
  print_string "> ";
  match read_line () with
  | input -> play_game input

(* Execute the game engine. *)
let () = main ()