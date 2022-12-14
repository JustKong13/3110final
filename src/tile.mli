type tile_type =
  | ATile
  | HTile
  | VTile
(* ATile are types of tiles that are 1x1. HTiles are types of tiles that are 1x2
   (horizontal). VTiles are types of tiles that are 2x1 (vertical). *)

type t = {
  tstring : string;
  ttype : tile_type;
  position : int * int;
}
(** Type t represents a tile, which has a string of 1 or 2 characters associated
    with it, a tile type of either ATile, BTile, or CTile, and a position (x ,
    y) on a game grid, where x represents its position on the x-axis, and y
    represents its position on the y-axis.*)

val tile_list : t list
(* [tile_list] is a list of tiles with random, valid positions that don't
   overlap.*)

val lst_string : t list -> string
(* [lst_string t_list] is the string representation of a list of tiles, which
   gives information regarding their string contents, thier alignment, and their
   position. *)

val adjacent : t -> int * int -> int * int
(* [adjacent t] gets the adjacent component that makes up a (1 x 2) or (2 x 1)
   tile t*)

val new_coords : t -> int * int -> t
(* [adjacent t (x , y)] changes tile t's position to (x , y). *)

val max_x : t -> int
(* [max_x t ] returns the maximum x-coordinate that t can be placed. *)

val max_y : t -> int
(* [max_y t ] returns the maximum y-coordinate that t can be placed. *)