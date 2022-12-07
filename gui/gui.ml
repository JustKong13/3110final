open Graphics
open Game
open Board

(* [get_board_coords (x, y)] converts graphical representation coordinates to
   board coordinates. Graphical coordinates range from (60-120, 540-600) to
   (480-540, 540-600) down to (60-120, 60-120) to (480-540, 60-120) with respect
   to board coordinates ranging from (0, 0) to (0, 7) down to (8, 0) to (8, 7).
   Graphical y converts to board x and graphical x converts to board y. *)
let get_board_coords (x, y) =
  if x >= 60 && y >= 60 && x <= 540 && y <= 600 then
    ((600 - y) / 60, (x - 60) / 60)
  else (-1, -1)

(* [get_grid_coords (x, y)] is the opposition conversion of
   [get_board_cords]. *)
let get_grid_coords (x, y) = ((y * 60) + 60, 540 - (x * 60))

let draw_none_selection () =
  set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1";
  moveto ((3 * size_x () / 4) - 10) (size_y () - 150);
  draw_string "Current tile selected: NONE"

let draw_selected_tile (x, y) () =
  set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1";
  moveto ((3 * size_x () / 4) - 10) (size_y () - 150);
  let board_coords = get_board_coords (x, y) in
  if
    fst board_coords >= 0
    && fst board_coords <= 8
    && snd board_coords >= 0
    && snd board_coords <= 7
  then
    draw_string
      ("Current tile selected: " ^ "("
      ^ string_of_int (fst board_coords)
      ^ ", "
      ^ string_of_int (snd board_coords)
      ^ ")")
  else draw_none_selection ()

let draw_single_letter c (x, y) =
  let grid_coords = get_grid_coords (x, y) in
  moveto (fst grid_coords + 20) (snd grid_coords + 4);
  draw_char c

let draw_all_letters (g : Wordbite.game) () =
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  let b = g.board in
  for x = 0 to 8 do
    for y = 0 to 7 do
      let c = Board.get_letter (x, y) b in
      if not (String.equal c "-") then draw_single_letter (String.get c 0) (x, y)
    done
  done

let draw_words_found (g : Wordbite.game) () =
  set_color black;
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  moveto (3 * size_x () / 4) (size_y () - 250);
  draw_string "Words Found"
(* add function to draw words_found for type [game]*)

let draw_gridlines () =
  set_color black;
  set_line_width 1;
  moveto 60 60;
  for v = 0 to 8 do
    lineto (current_x ()) (current_y () + 540);
    moveto (current_x () + 60) 60
  done;
  moveto 60 60;
  for v = 0 to 9 do
    lineto (current_x () + 480) (current_y ());
    moveto 60 (current_y () + 60)
  done

let draw_game (g : Wordbite.game) () =
  draw_gridlines ();
  draw_words_found g ();
  draw_all_letters g ()

let draw_title_screen () =
  draw_string "Press \'p\' to start the game";
  draw_string "Press \'q\' to quit"

let create_window =
  open_graph "";
  resize_window 800 700;
  set_window_title "Wordbite";
  draw_title_screen