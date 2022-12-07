open Graphics
open Game
open Board

exception InvalidString

(** [exit_game] bids farewell to the player and closes the graphics window. *)
let exit_game () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n\nThanks for playing Wordbite!";
  exit 0

(** [check_bounds pair] returns if the tuple [pair] is valid. A [pair] is valid
    if x >= 0 && x <= 8 && y >= 0 && y <= 7. *)
let check_bounds pair =
  fst pair >= 0 && fst pair <= 8 && snd pair >= 0 && snd pair <= 7

(** [turn_handler g] handles player input to select a tile and make changes to
    the board. *)
let rec turn_handler (g : Wordbite.game) () =
  let event = wait_next_event [ Button_down; Key_pressed ] in
  match (event.keypressed, event.button) with
  | true, _ -> (
      match event.key with
      | 'q' | 'Q' -> exit_game ()
      | _ -> turn_handler g ())
  | false, true -> (
      match event.button with
      | true ->
          clear_graph ();
          Gui.draw_game g ();
          Gui.draw_selected_tile (event.mouse_x, event.mouse_y) ();
          let coords = Gui.get_board_coords (event.mouse_x, event.mouse_y) in
          if check_bounds coords then turn_handler_swap g coords ()
          else turn_handler g ()
      | false -> turn_handler g ())
  | _ -> turn_handler g ()

(** [turn_handler_swap g] handles swapping tiles between the selected tile and
    the curernt tile that the player is hovering over. *)
and turn_handler_swap (g : Wordbite.game) (initial_coords : int * int) () =
  let event = wait_next_event [ Key_pressed ] in
  match event.key with
  | 'q' | 'Q' -> exit_game ()
  | 's' | 'S' ->
      moveto event.mouse_x event.mouse_y;
      let new_coords = Gui.get_board_coords (event.mouse_x, event.mouse_y) in
      if check_bounds new_coords then
        try
          let new_board =
            Wordbite.move_on_board initial_coords new_coords g.board
          in
          let new_g = { g with board = new_board } in
          clear_graph ();
          Gui.draw_game new_g ();
          Gui.draw_none_selection ();
          turn_handler new_g ()
        with Failure _ -> turn_handler_swap g initial_coords ()
      else turn_handler_swap g initial_coords ()
  | _ -> turn_handler_swap g initial_coords ()

(** [game_handler] handles player input to play and quit the game. *)
let rec game_handler () =
  let event = wait_next_event [ Key_pressed ] in
  match event.key with
  | 'q' | 'Q' -> exit_game ()
  | 'p' | 'P' ->
      let g = Wordbite.init_game in
      Gui.draw_game g ();
      Gui.draw_none_selection ();
      turn_handler g ()
  | _ -> game_handler ()

(** [play_game] starts the game if the player types ["start"] in the terminal. *)
let rec play_game () =
  print_string "\nPlease type \'start\' to start the game.\n";
  print_string "> ";
  try
    let input = String.lowercase_ascii (read_line ()) in
    match input with
    | "start" ->
        Gui.create_window ();
        game_handler ()
    | _ -> raise InvalidString
  with InvalidString ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\nYou did not type in \'start\' correctly...";
    play_game ()

(** [main] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "\n\nWelcome to Wordbite!\n";
  play_game ()

(* Execute the game engine. *)
let () = main ()