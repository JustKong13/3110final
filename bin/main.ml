(* This file was referenced using FA22 CS 3110 A2 assignment. *)

(** [play_game s] starts the Wordbite game. *)
let play_game s = raise (Failure "Unimplemented: Main.play_game")
(* 1. Generate lists of string of letters *)
(* 2. Call a recursive function to prompt user to enter in a string *)
(* 3. Parse the string -> if string consists of strings from list, then check if
   it is a word *)
(* 4. Prints error message if string is considered Illegal *)
(* 5. Add string to list of checked words *)
(* 6. Repeat from step 2 *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "\n\nWelcome to Wordbite!.\n";
  print_endline "Please type start.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | string -> play_game string

(* Execute the game engine. *)
let () = main ()