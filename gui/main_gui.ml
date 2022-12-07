open Graphics

exception InvalidString

(** [exit_game] bids farewell to the player and closes the graphics window. *)
let exit_game () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n\nThanks for playing Wordbite!";
  exit 0

(** [check_bounds pair] returns if the tuple [pair] is valid. A [pair] is valid
    if x >= 0 && x <= 7 && y >= 0 && y <= 7. *)
let check_bounds pair =
  fst pair >= 0 && fst pair <= 7 && snd pair >= 0 && snd pair <= 7

(** [turn_handler] handles player input to select a tile and make changes to the
    board. *)
let rec turn_handler () =
  let event = wait_next_event [ Button_down; Key_pressed ] in
  match (event.keypressed, event.button) with
  | true, _ -> (
      match event.key with
      | 'q' | 'Q' -> exit_game ()
      | _ -> turn_handler ())
  | false, true -> (
      match event.button with
      | true ->
          Graphics.clear_graph ();
          Gui.draw_game ();
          Gui.draw_selected_tile (event.mouse_x, event.mouse_y) ();
          let coords = Gui.get_board_coords (event.mouse_x, event.mouse_y) in
          if check_bounds coords then turn_handler_swap () else turn_handler ()
      | false -> turn_handler ())
  | _ -> turn_handler ()

(** [turn_handler_swap] handles swapping tiles between the selected tile and the
    curernt tile that the player is hovering over. *)
and turn_handler_swap () =
  let event = wait_next_event [ Key_pressed ] in
  match event.key with
  | 'q' | 'Q' -> exit_game ()
  | 's' | 'S' ->
      Graphics.moveto event.mouse_x event.mouse_y;
      let coords = Gui.get_board_coords (event.mouse_x, event.mouse_y) in
      if check_bounds coords then (
        draw_string
          (string_of_int (fst coords) ^ " " ^ string_of_int (snd coords));
        turn_handler ())
      else turn_handler_swap ()
  | _ -> turn_handler_swap ()

(** [game_handler] handles player input to play and quit the game. *)
let rec game_handler () =
  let event = wait_next_event [ Key_pressed ] in
  match event.key with
  | 'q' | 'Q' -> exit_game ()
  | 'p' | 'P' ->
      Gui.draw_game ();
      Gui.draw_none_selection ();
      turn_handler ()
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