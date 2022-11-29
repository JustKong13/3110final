open Graphics

let get_board_coords (x, y) = ((x - 60) / 60, -((y / 60) - 8))
let get_grid_coords (x, y) = ((x * 60) + 60, 480 - (y * 60))

let draw_selected_tile (x, y) () =
  set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1";
  moveto ((3 * size_x () / 4) - 10) (size_y () - 100);
  let board_coords = get_board_coords (x, y) in
  draw_string
    ("Current tile selected: " ^ "("
    ^ string_of_int (fst board_coords)
    ^ ", "
    ^ string_of_int (snd board_coords)
    ^ ")")

let draw_initial_tile_selected () =
  set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1";
  moveto ((3 * size_x () / 4) - 10) (size_y () - 100);
  draw_string "Current tile selected: NONE"

(* change to draw_char*)
let draw_single_letter c (x, y) =
  let grid_coords = get_grid_coords (x, y) in
  moveto (fst grid_coords + 20) (snd grid_coords + 4);
  draw_string c

(* change to draw all letters in the board *)
let draw_all_letters () =
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  for x = 0 to 7 do
    for y = 0 to 7 do
      let temp = string_of_int (x + y) in
      draw_single_letter temp (x, y)
    done
  done

let draw_words_found () =
  set_color black;
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  moveto (3 * size_x () / 4) (size_y () - 200);
  draw_string "Words Found"
(* add function to draw words_found for type [game]*)

let draw_gridlines () =
  set_color black;
  set_line_width 1;
  moveto 60 60;
  for v = 0 to 8 do
    lineto (current_x ()) (current_y () + 480);
    moveto (current_x () + 60) 60
  done;
  moveto 60 60;
  for v = 0 to 8 do
    lineto (current_x () + 480) (current_y ());
    moveto 60 (current_y () + 60)
  done

let draw_game () =
  draw_gridlines ();
  draw_words_found ();
  draw_all_letters ()

let draw_title_screen () =
  draw_string "Press \'p\' to start the game";
  draw_string "Press \'q\' to quit"

let create_window =
  open_graph "";
  resize_window 800 650;
  set_window_title "Wordbite";
  draw_title_screen