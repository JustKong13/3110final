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

let rec print_lst words =
  match words with
  | [] -> ""
  | h :: t -> h ^ ", " ^ print_lst t

(** [set_up_game] initializes the game state for the user. *)
let set_up_game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "The list of usable letters are:\n";
  print_endline
    (print_lst (available_strs "aabcdeefghiijklmnoopqrstuuvwxyz" []));
  print_endline "Type in a word constructed by the letters in the list above\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | input -> parse_input input

(** [play_game input] starts the Wordbite game if [input] is "start". *)
let rec play_game input =
  try
    match input with
    | "start" -> set_up_game
    | _ -> raise InvalidString
  with InvalidString -> (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\nYou did not type in \'start\' correctly...\n";
    print_endline "Please type \'start\' to start the game.\n";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | input -> play_game input)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "\n\nWelcome to Wordbite.\n";
  print_endline "Please type \'start\' to start the game.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | input -> play_game input

(* Execute the game engine. *)
let () = main ()