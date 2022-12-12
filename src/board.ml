module Board = struct
  (** Each square on the grid will either be nothing or a stringacer*)
  type letter =
    | None
    | Some of string

  type t = letter list list
  (** Representation type of a board, 2D array of letters*)

  type coord = int * int
  (** [coord] is a provided coordinate on the board where (x,y) is (0,0) at the
      top leftmost position and (8,7) is the bottom right most position*)

  (** [empty] returns the empty list*)
  let empty = List.init 9 (fun _ -> List.init 8 (fun _ -> None))

  (** [get_letter] gets the letter at the coordinates (x,y) on the board b*)
  let get_letter ((x, y) : coord) (b : t) =
    match List.nth (List.nth b x) y with
    | Some c -> c
    | None -> "-"

  let is_empty ((x, y) : coord) (b : t) =
    match List.nth (List.nth b x) y with
    | Some _ -> false
    | None -> true

  (** [place_in_row] takes a string [a] and places it in the [x]th position in a
      row *)
  let rec place_in_row (a : string) (x : int) (row : letter list) =
    match row with
    | [] -> row
    | h :: t -> if x = 0 then Some a :: t else h :: place_in_row a (x - 1) t

  (** [place_letter a (x, y) b] places a letter [a] at the coordinates [(x, y)]
      on the board [b]*)
  let rec place_letter (a : string) ((x, y) : coord) (b : t) =
    match b with
    | [] -> []
    | h :: t ->
        if y = 0 then place_in_row a x h :: t
        else h :: place_letter a (x, y - 1) t

  let rec remove_letter_in_row (x : int) (row : letter list) =
    match row with
    | [] -> row
    | h :: t -> if x = 0 then None :: t else h :: remove_letter_in_row (x - 1) t

  let rec remove_letter (x, y) (b : t) =
    match b with
    | [] -> []
    | h :: t ->
        if y = 0 then remove_letter_in_row x h :: t
        else h :: remove_letter (x, y - 1) t

  let rec row_to_list (row : letter list) =
    match row with
    | [] -> []
    | h :: t -> begin
        match h with
        | None -> "-" :: row_to_list t
        | Some a -> a :: row_to_list t
      end

  let rec board_to_list (b : t) =
    match b with
    | [] -> []
    | h :: t -> row_to_list h :: board_to_list t
end
