module Board = struct
  (** Each square on the grid will either be nothing or a characer*)
  type letter =
    | None
    | Some of char

  type t = letter list list
  (** Representation type of a board, 2D array of letters*)

  type coord = int * int
  (** [coord] is a provided coordinate on the board where (x,y) is (0,0) at the
      top leftmost position and (7,8) is the bottom right most position*)

  (** [empty] returns the empty list*)
  let empty = List.init 9 (fun _ -> List.init 8 (fun _ -> None))

  (** [get_letter] gets the letter at the coordinates (x,y) on the board b*)
  let get_letter ((x, y) : coord) (b : t) = List.nth (List.nth b x) y

  (** [place_in_row] takes a character [a] and places it in the [y]th position
      in a row *)
  let rec place_in_row (a : char) (y : int) (row : letter list) =
    match row with
    | [] -> row
    | h :: t -> if y = 0 then Some a :: t else h :: place_in_row a (y - 1) t

  (** [place_letter a (x, y) b] places a letter [a] at the coordinates [(x, y)]
      on the board [b]*)
  let place_letter (a : char) ((x, y) : coord) (b : t) =
    match b with
    | [] -> []
    | h :: t -> assert false
end
