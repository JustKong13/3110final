(** Interface for GUI using OCaml graphics

    This module allows other modules to create or update information to display
    to the user. *)

val create_window : unit
(** [create_window] opens the graphics window. *)

val get_board_coords : int * int -> int * int
(** [get_board_coords (x, y)] converts graphical representation coordinates to
    board coordinates. Graphical coordinates range from (60-120, 540-600) to
    (480-540, 540-600) down to (60-120, 60-120) to (480-540, 60-120) with
    respect to board coordinates ranging from (0, 0) to (7, 0) down to (0, 8) to
    (7, 8). Graphical y converts to board x and graphical x converts to board y. *)

val get_grid_coords : int * int -> int * int
(** [get_grid_coords (x, y)] is the opposite conversion from board coordinates
    to graphical coordinates of [get_board_cords (x', y')]. *)

val draw_game : Game.Wordbite.game -> unit -> unit
(** [draw_game g] draws the current game onto the GUI. The user is displayed
    information about:

    - all the tiles on the board
    - their current score
    - the words they have found
    - the current tile they selected. *)

val draw_selected_tile : int * int -> unit -> unit
(** [draw_selected_tile (x, y)] draws the coordinates of the tile that the user
    selected. *)

val draw_none_selection : unit -> unit
(** [draw_none_selection] draws ["None"] if the user has not selected a tile
    yet. *)

val draw_error_msg : string -> unit -> unit
(** [draw_error_msg s] draws the error message [s] in red to display to the user
    when an exception is raised. *)

val draw_instructions : unit -> int * int * int * int
(** [draw_instructions] draws the instructions on the title screen and returns
    the boundaries of the text. *)

val draw_title_screen :
  unit -> int * int * int * int * int * int * int * int * int * int * int * int
(** [draw_title_screen] draws the title screen with ["Play"], ["Instructions"],
    ["Quit"] texts and returns the boundaries of those texts. *)
