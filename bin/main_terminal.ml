(* This file was referenced using FA22 CS 3110 A2 assignment. *)

open Game.Wordbite
open Game.Wordvalidator

exception InvalidString

(** [parse_input s words] parses [s] checking if it is made up of strings from
    [words] and checks whether [s] is a real word, otherwise prompts the user
    for another input. *)
let rec parse_input s = ()
(* 3. Parse the string -> if string consists of strings from list, then check if
   it is a word *)
(* 4. Prints error message if string is considered Illegal *)
(* 5. Add string to list of checked words *)
(* 6. Repeat from step 2 *)

module G = Game.Wordbite

let game_state = G.init_game

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
           (`x1` `y1`) (`x2` `y2`)";
        print_string "> ";
        match read_line () with
        | input -> parse_input input)
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
  ANSITerminal.print_string [ ANSITerminal.cyan ] "\n\nWelcome to Wordbite.\n";
  print_endline "Please type \'start\' to start the game.\n";
  print_string "> ";
  match read_line () with
  | input -> play_game input

(* Execute the game engine. *)
let () = main ()