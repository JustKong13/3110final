(* Referenced from CS 3110 A2 assignment *)

(** [play_game] starts the wordbite game *)
let play_game = raise (Failure "Unimplemented: Main.play_game")

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Wordbite.\n";
  print_endline "Please type play\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | string -> play_game

(* Execute the game engine. *)
let () = main ()