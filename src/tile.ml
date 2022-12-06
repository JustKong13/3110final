Random.self_init ()

type tile_type =
  | ATile
  | HTile
  | VTile

type t = {
  tstring : string;
  ttype : tile_type;
  position : int * int;
}

let string_type (t : t) =
  match t.ttype with
  | ATile -> "ATile"
  | HTile -> "HTile"
  | VTile -> "VTile"

let string_pos (t : t) =
  match t.position with
  | x, y -> "(" ^ string_of_int x ^ " , " ^ string_of_int y ^ ")"

let tile_string (t : t) =
  "[ TILE INFO: " ^ "string: " ^ t.tstring ^ " | type: " ^ string_type t
  ^ " | position : " ^ string_pos t ^ "]"

let rec lst_string (lst : t list) =
  match lst with
  | [] -> []
  | h :: t -> tile_string h :: lst_string t

let get_x pair =
  match pair with
  | x, _ -> x

let get_y pair =
  match pair with
  | _, y -> y

let max_x (t : t) =
  match t.ttype with
  | ATile -> 7
  | HTile -> 6
  | VTile -> 7

let max_y (t : t) =
  match t.ttype with
  | ATile -> 8
  | HTile -> 8
  | VTile -> 7

let create_aux (s : string) =
  if String.length s = 1 then { tstring = s; ttype = ATile; position = (0, 0) }
  else if Random.int 2 = 0 then
    { tstring = s; ttype = HTile; position = (0, 0) }
  else { tstring = s; ttype = VTile; position = (0, 0) }

let rec create (s : string list) (acc : t list) =
  match s with
  | [] -> acc
  | h :: t -> create_aux h :: create t acc

let new_coords (t : t) (coord : int * int) =
  { tstring = t.tstring; ttype = t.ttype; position = coord }

let adjacent (t : t) (pair : int * int) =
  if t.ttype = ATile then (get_x pair, get_y pair)
  else if t.ttype = HTile then (1 + get_x pair, get_y pair)
  else (get_x pair, 1 + get_y pair)

let get_full pair =
  match pair with
  | _, f -> f

let get_tile pair =
  match pair with
  | t, _ -> t

let rec place (t_lst : t list) (full : (int * int) list) =
  let rec place_aux (t, full) =
    let x = Random.int (max_x t + 1) in
    let y = Random.int (max_y t) + 1 in
    if List.mem (x, y) full || List.mem (adjacent t (x, y)) full then
      place_aux (t, full)
    else (new_coords t (x, y), (x, y) :: adjacent t (x, y) :: full)
  in
  match t_lst with
  | [] -> []
  | h :: t ->
      let pair = place_aux (h, full) in
      get_tile pair :: place t (get_full pair)