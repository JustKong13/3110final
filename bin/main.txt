open Game.Wordbite
open Game.Wordvalidator
open Graphics

exception InvalidString

(** [sort s] sorts string [s] in alphabetical order. *)
let sort s =
  let n = String.length s in
  let a = Array.init n (fun i -> s.[i]) in
  Array.sort Char.compare a;
  String.init n (fun i -> a.(i))

(** [contains s1 s2] checks whether string [s2] is a substring of string [s1]. *)
let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false

(** [form_word s words acc] iterates over string list [words] and checks whether
    each string is a substring of string [s]. Returns a string of all of the
    elements in [words] that are substrings in [s]. *)
let rec form_word s words acc =
  match words with
  | [] -> acc
  | h :: t -> if contains s h then form_word s t acc ^ h else form_word s t acc

(** [print_lst words] prints [lst] in a readable format. *)
let rec print_lst words =
  match words with
  | [] -> ""
  | [ h ] -> h
  | h :: t -> h ^ ", " ^ print_lst t

(** [parse_input s words] parses [s] by checking if it is constructed by
    concatenation of strings in list [words] and checks whether [s] is a real
    word, otherwise prompts the user for another input. *)
let rec parse_input s words =
  try
    if
      check_word s
      && (String.equal (sort (form_word s words "")) (sort s)
         || contains (sort (form_word s words "")) (sort s)
         || contains (form_word s words "") s)
    then (
      ANSITerminal.print_string [ ANSITerminal.green ] ("Word valid! " ^ s);
      print_endline "\nType another word!\n";
      print_string "> ";
      parse_input (read_line ()) words)
    else raise InvalidString
  with InvalidString ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\n\nYou entered:" ^ s
     ^ "\n\
        Unfortunately, it does not consist of strings in the list or is not a \
        real word.\n");
    print_string "[";
    print_string (print_lst words);
    print_endline "]";
    print_endline
      "\nType in a word constructed by the letters in the list above\n";
    print_string "> ";
    parse_input (read_line ()) words

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