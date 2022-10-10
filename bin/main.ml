(* This file was referenced from CS 3110 A2 assignment. *)

(** [play_game] starts the wordbite game. *)
let play_game = raise (Failure "Unimplemented: Main.play_game")
(* 1. Generate lists of string of letters *)
(* 2. Call a recursive function to prompt user to enter in a string *)
(* 3. Parse the string -> if string consists of strings from list, then check if
   it is a word *)
(* 4. Prints error message if string is considered Illegal *)
(* 5. Add string to list of checked words *)
(* 6. Repeat from step 2 *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Wordbite.\n";
  print_endline "Please type start\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | string ->
      if string = "start" then play_game
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\n\nPlease type start correctly...\n"

(* Execute the game engine. *)
let () = main ()