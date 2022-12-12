open Graphics
open Game
open Board

(* [get_board_coords (x, y)] converts graphical representation coordinates to
   board coordinates. Graphical coordinates range from (60-120, 540-600) to
   (480-540, 540-600) down to (60-120, 60-120) to (480-540, 60-120) with respect
   to board coordinates ranging from (0, 0) to (7, 0) down to (0, 8) to (7, 8).
   Graphical y converts to board x and graphical x converts to board y. *)
let get_board_coords (x, y) =
  if x >= 60 && y >= 60 && x <= 540 && y <= 600 then
    ((x - 60) / 60, (600 - y) / 60)
  else (-1, -1)

(* [get_grid_coords (x, y)] is the opposite conversion from board coordinates to
   graphical coordinates of [get_board_cords (x', y')]. *)
let get_grid_coords (x, y) = ((x * 60) + 60, 540 - (y * 60))

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
    && fst board_coords <= 7
    && snd board_coords >= 0
    && snd board_coords <= 8
  then
    draw_string
      ("Current tile selected: " ^ "("
      ^ string_of_int (fst board_coords)
      ^ ", "
      ^ string_of_int (snd board_coords)
      ^ ")")

let draw_single_letter c (x, y) =
  let grid_coords = get_grid_coords (x, y) in
  moveto (fst grid_coords + 20) (snd grid_coords + 4);
  draw_char c

let draw_all_letters (g : Wordbite.game) () =
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  let b = g.board in
  for lst = 0 to 8 do
    for letter = 0 to 7 do
      let c = Board.get_letter (lst, letter) b in
      if not (String.equal c "-") then
        let x = letter in
        let y = lst in
        draw_single_letter (String.get c 0) (x, y)
    done
  done

let draw_score (g : Wordbite.game) () =
  set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
  moveto ((3 * size_x () / 4) - 10) (size_y () - 200);
  draw_string ("Current Score: " ^ string_of_int g.score)

let draw_words_found (g : Wordbite.game) () =
  set_color black;
  set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  moveto ((3 * size_x () / 4) - 10) (size_y () - 250);
  draw_string "Words Found";
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  for v = 1 to List.length g.words_found do
    moveto ((3 * size_x () / 4) - 10) (size_y () - 280 - (v * 25));
    draw_string (List.nth g.words_found (v - 1))
  done

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
  draw_score g ();
  draw_all_letters g ()

let draw_instructions () =
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  let instr_dim = text_size "Instructions" in
  moveto ((size_x () / 2) - (fst instr_dim / 2)) (size_y () - 125);
  draw_string "Instructions";
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  moveto (size_x () / 10) (size_y () - 180);
  set_color (rgb 0 171 65);
  let text_dim = text_size "Objective: " in
  draw_string "Objective: ";
  set_color black;
  draw_string "Create as many words as you can by moving";
  moveto ((size_x () / 10) + fst text_dim) (size_y () - 180 - snd text_dim);
  draw_string "tiles around.";
  set_color (rgb 179 139 109);
  moveto (size_x () / 10) (size_y () - 230 - snd text_dim);
  draw_string "Good Things to Know";
  set_color black;
  let arrow_dim = text_size "-> " in
  moveto (size_x () / 10) (size_y () - 260 - snd text_dim);
  draw_string "-> Letters may be a (1 x 1) tile or a tile pair aligned";
  moveto ((size_x () / 10) + fst arrow_dim) (size_y () - 285 - snd text_dim);
  draw_string "vertically (2 x 1) or horizontally (1 x 2).";
  moveto (size_x () / 10) (size_y () - 325 - snd text_dim);
  draw_string "-> You may only move tiles to empty spaces on the board.";
  moveto ((size_x () / 10) + fst arrow_dim) (size_y () - 350 - snd text_dim);
  draw_string "They CANNOT go out of bounds.";
  set_color red;
  moveto (size_x () / 10) (size_y () - 400 - snd text_dim);
  draw_string "The Controls";
  set_color black;
  moveto (size_x () / 10) (size_y () - 430 - snd text_dim);
  draw_string "Mouse Click - select a tile with a letter to move around";
  moveto (size_x () / 10) (size_y () - 460 - snd text_dim);
  draw_string "Spacebar - place selected tile into valid empty tiles";
  moveto (size_x () / 10) (size_y () - 490 - snd text_dim);
  draw_string "Escape Key - deselect tile to select a new tile"

let draw_title_screen () =
  set_font "-*-fixed-medium-r-semicondensed--75-*-*-*-*-*-iso8859-1";
  let title_dim = text_size "Wordbites" in
  moveto ((size_x () / 2) - (fst title_dim / 2)) ((size_y () / 2) + 120);
  draw_string "Wordbites";
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  let play_dim = text_size "Play" in
  moveto ((size_x () / 2) - (fst play_dim / 2)) ((size_y () / 2) - 20);
  draw_string "Play";
  let instructions_dim = text_size "Instructions" in
  moveto ((size_x () / 2) - (fst instructions_dim / 2)) ((size_y () / 2) - 120);
  draw_string "Instructions";
  let quit_dim = text_size "Quit" in
  moveto ((size_x () / 2) - (fst quit_dim / 2)) ((size_y () / 2) - 220);
  draw_string "Quit";
  let play_l = (size_x () / 2) - (fst play_dim / 2) in
  let play_r = (size_x () / 2) + (fst play_dim / 2) in
  let play_t = (size_y () / 2) - 20 + snd play_dim in
  let play_b = (size_y () / 2) - 20 in
  let instr_l = (size_x () / 2) - (fst instructions_dim / 2) in
  let instr_r = (size_x () / 2) + (fst instructions_dim / 2) in
  let instr_t = (size_y () / 2) - 120 + snd instructions_dim in
  let instr_b = (size_y () / 2) - 120 in
  let quit_l = (size_x () / 2) - (fst quit_dim / 2) in
  let quit_r = (size_x () / 2) + (fst quit_dim / 2) in
  let quit_t = (size_y () / 2) - 220 + snd quit_dim in
  let quit_b = (size_y () / 2) - 220 in
  ( play_l,
    play_r,
    play_t,
    play_b,
    instr_l,
    instr_r,
    instr_t,
    instr_b,
    quit_l,
    quit_r,
    quit_t,
    quit_b )

let create_window =
  open_graph "";
  resize_window 800 700;
  set_window_title "Wordbite";
  draw_title_screen