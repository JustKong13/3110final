type letter =
  | None
  | Some of string
      (** [letter] represents a tile component, so either a 1 x 1 tile itself,
          or part of a 1 x 2 or 2 x 1 tile. *)

type t = letter list list
(** Type t represents a board, which is a 8 x 9 2D array of either tile
    components or associated with it, a tile type of either ATile, BTile, or
    CTile, and a position (x,y) on a Cartesian game grid, where x represents its
    position on the x-axis, and y represents its position on the y-axis.*)

val empty : t
(** This represents the empty board. *)

val is_empty : int * int -> t -> bool
(** [is_empty (x,y) t] returns true if the space on the board at (x, y) is empty
    and false if not*)

val get_letter : int * int -> t -> string
(** [get_letter (x, y) b] gets the letter of the tile at (x, y) on the board [b]*)

val place_letter : string -> int * int -> t -> letter list list
(** [place_letter a (x, y) b] places a letter [a] at (x, y) on the board [b] and
    returns the new board.*)

val remove_letter : int * int -> t -> letter list list
(** [remove_letter (x, y) b] removes a letter at (x, y) on the board [b] and
    returns the new board.*)

val board_to_list : t -> string list list
(** [board_to_list t] returns the tile list representation of the board. *)