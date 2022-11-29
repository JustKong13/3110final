type tile_type =
  | ATile
  | BTile
  | CTile
(* ATile are types of tiles that are 1x1. BTiles are types of tiles that are 1x2
   (horizontal). CTiles are types of tiles that are 2x1 (vertical). *)

type t
(** Type t repressents a tile, which has a string of 1 or 2 characters
    associated with it, a tile type of either ATile, BTile, or CTile, and a
    position (x,y) on a game grid, where x represents its position on the
    x-axis, and y represents its position on the y-axis.*)

val create : string list -> t list -> t list
(* [create s acc] instantiates a list of tiles with positions at (0,0) from the
   string list.*)

val place : t list -> (int * int) list -> t list
(* [place tlist] gives a list of tiles random, valid positions that don't
   overlap.*)
