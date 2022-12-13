open Graphics
open Game
open Board
open Wordbite

exception InvalidString

(** [exit_game] bids farewell to the user and terminates the graphics window. *)
let exit_game () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\nThanks for playing Wordbite!";
  exit 0

(** [check_bounds pair] returns if the tuple [pair] is valid. A [pair] is valid
    if x >= 0 && x <= 7 && y >= 0 && y <= 8. *)
let check_bounds pair =
  fst pair >= 0 && fst pair <= 7 && snd pair >= 0 && snd pair <= 8

(** [turn_handler g] handles player mouse input to select a tile on the board *)
let rec turn_handler (g : Wordbite.game) () =
  let event = wait_next_event [ Button_down ] in
  match event.button with
  | true ->
      let coords = Gui.get_board_coords (event.mouse_x, event.mouse_y) in
      if check_bounds coords then (
        clear_graph ();
        Gui.draw_game g ();
        Gui.draw_selected_tile (event.mouse_x, event.mouse_y) ();
        turn_handler_place g coords ())
      else turn_handler g ()
  | false -> turn_handler g ()

(** [turn_handler_place g initial_coords] allows the user to place the selected
    tiles associated with [initial_coords] onto valid empty spaces on the board.
    The user can press the ["ESC"] key to deselect the initally selected title.
    The user can press the ["SPACE"] key to place the selected tiles onto valid
    empty tiles. *)
and turn_handler_place (g : Wordbite.game) (initial_coords : int * int) () =
  let event = wait_next_event [ Key_pressed ] in
  match event.key with
  (* escape key *)
  | '\027' ->
      clear_graph ();
      Gui.draw_game g ();
      Gui.draw_none_selection ();
      turn_handler g ()
  (* space key *)
  | '\032' ->
      let hovered_coords =
        Gui.get_board_coords (event.mouse_x, event.mouse_y)
      in
      if check_bounds hovered_coords then
        try
          let new_tile_lst =
            Wordbite.move initial_coords hovered_coords g.tile_list
          in
          let new_board =
            Wordbite.generate_game_board new_tile_lst Board.empty
          in
          let new_g = g in
          new_g.tile_list <- new_tile_lst;
          new_g.board <- new_board;
          Wordbite.new_move
            (Wordbite.check_for_words hovered_coords new_g)
            new_g;
          clear_graph ();
          Gui.draw_game new_g ();
          Gui.draw_none_selection ();
          turn_handler new_g ()
        with excn -> (
          match excn with
          | TileNotFound ->
              Gui.draw_error_msg "empty tile" ();
              turn_handler g ()
          | OutOfBound ->
              Gui.draw_error_msg "out of bounds" ();
              turn_handler_place g initial_coords ()
          | TileOverlap ->
              Gui.draw_error_msg "tile overlap" ();
              turn_handler_place g initial_coords ()
          | _ -> exit_game ())
      else turn_handler_place g initial_coords ()
  | _ -> turn_handler_place g initial_coords ()

(** [title_handler title_dimensions] handles player mouse input on the title
    screen to play the game, read the instructions, or quit the game.
    [title_dimensions] contains the centered left, right, top, and bottom
    coordinates of the texts: ["Play"],["Instructions"], and ["Quit"] on the
    title screen. *)
let rec title_handler title_dimensions () =
  let event = wait_next_event [ Button_down ] in
  match event.button with
  | true -> (
      match title_dimensions with
      | pl, pr, pt, pb, il, ir, it, ib, ql, qr, qt, qb ->
          let x = event.mouse_x in
          let y = event.mouse_y in
          if x >= pl && x <= pr && y <= pt && y >= pb then (
            let g = Wordbite.init_game in
            clear_graph ();
            Gui.draw_game g ();
            Gui.draw_none_selection ();
            turn_handler g ())
          else if x >= il && x <= ir && y <= it && y >= ib then (
            clear_graph ();
            let instr_dimensions = Gui.draw_instructions () in
            instr_handler instr_dimensions ())
          else if x >= ql && x <= qr && y <= qt && y >= qb then exit_game ()
          else title_handler title_dimensions ())
  | false -> title_handler title_dimensions ()

(** [instr_handler instr_dimensions] handles player mouse input on the
    instructions screen. [instr_dimensions] contains the centered left, right,
    top, and bottom coordinates of the text ["Back to Main Menu"]. Returns to
    the title screen when the text is clicked. *)
and instr_handler instr_dimensions () =
  let event = wait_next_event [ Button_down ] in
  match event.button with
  | true -> (
      match instr_dimensions with
      | il, ir, it, ib ->
          let x = event.mouse_x in
          let y = event.mouse_y in
          if x >= il && x <= ir && y <= it && y >= ib then (
            clear_graph ();
            let title_dimensions = Gui.draw_title_screen () in
            title_handler title_dimensions ())
          else instr_handler instr_dimensions ())
  | false -> instr_handler instr_dimensions ()

(** [play_game] starts the game if the user types ["start"] in the terminal. *)
let rec play_game () =
  print_string "\nPlease type \'start\' to start the game.\n";
  print_string "> ";
  try
    let input = String.lowercase_ascii (read_line ()) in
    match input with
    | "start" ->
        Gui.create_window;
        let title_dimensions = Gui.draw_title_screen () in
        title_handler title_dimensions ()
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