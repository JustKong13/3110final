open Graphics

let test_draw_board () =
  set_color black;
  set_line_width 0;
  moveto 50 150;
  for v = 0 to 8 do
    lineto (current_x ()) (current_y () + 300);
    moveto (current_x () + 40) 150
  done;
  moveto 50 150;
  for v = 0 to 9 do
    lineto (current_x () + 360) (current_y ());
    moveto 50 (current_y () + 40)
  done

let draw_words_found () =
  set_color cyan;
  fill_rect (3 * size_x () / 4) ((size_y () / 2) - 25) 50 50;
  set_color black;
  moveto ((3 * size_x () / 4) + 10) ((size_y () / 2) + 20);
  draw_string "Words found"

let create_window =
  open_graph "";
  resize_window 800 600;
  set_window_title "Wordbite";
  set_color black;
  test_draw_board ();
  draw_words_found ()
