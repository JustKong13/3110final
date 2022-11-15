open Graphics

(* let get_grid_xy x y = *)

(* [ (0, 0); ...; (0, 7) (1, 0); ...; (1, 7) ... (7, 0); ...; (7, 7)

   (7, 0) x goes from 60 - 120 y goes from 60 - 120

   (7, 1) x goes from 120 - 180 y goes from 60 - 120

   (6, 0) x goes from 60 - 120 y goes from 120 - 180 ] *)

(* draw the letterat x+20 and y+4 *)
let draw_letters () =
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  moveto 80 64;
  for v = 0 to 8 do
    draw_char 'C';
    moveto (80 + 60) 64
  done

let rec draw_usable_letters lst =
  match lst with
  | [] -> ()
  | h :: t ->
      draw_string (h ^ ", ");
      draw_usable_letters t

let draw_empty_board () =
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

let draw_words_found () =
  set_color black;
  moveto (3 * size_x () / 4) (size_y () - 130);
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  draw_string "Words Found"

let create_window =
  open_graph "";
  resize_window 800 650;
  set_window_title "Wordbite";
  draw_string "Press \'p\' to start the game";
  draw_string "Press \'q\' to quit"