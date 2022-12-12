Random.self_init ()

type tile_type =
  | ATile
  | HTile
  | VTile

(*type color = | Red | Blue | Yellow *)

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

(*Generate valid word list*)
let banned =
  let ic = open_in "./src/banned.txt" in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  loop []

let get_letter a_string =
  String.make 1 (String.get a_string (Random.int (String.length a_string)))

let rec strings_aux a_string =
  let size = Random.int 2 in
  if size = 0 then get_letter a_string
  else
    let first = get_letter a_string in
    let new_alphabet_string =
      Str.global_replace (Str.regexp first) "" a_string
    in
    let second_letter = get_letter new_alphabet_string in
    if List.mem (first ^ second_letter) banned then strings_aux a_string
    else first ^ second_letter

let double_vowels = "aabcdeefghiijklmnoopqrstuuvwxyz"

let rec strings a_string acc =
  if List.length acc < 10 then
    let new_str = strings_aux a_string in
    strings a_string (new_str :: acc)
  else acc

let string_pos (t : t) =
  match t.position with
  | x, y -> "(" ^ string_of_int x ^ " , " ^ string_of_int y ^ ")"

let type_to_align (t : t) =
  match t.ttype with
  | ATile -> "1 x 1"
  | HTile -> "1 x 2"
  | VTile -> "2 x 1"

let letter_helper (t : t) =
  match t.ttype with
  | ATile -> t.tstring ^ " "
  | _ -> t.tstring

let tile_string (t : t) =
  "[ " ^ letter_helper t ^ " | " ^ type_to_align t ^ " | " ^ string_pos t ^ " ]"

let rec lst_string (lst : t list) =
  match lst with
  | [] -> ""
  | h :: t -> tile_string h ^ "\n" ^ lst_string t

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

let full_neighbors (x, y) (full : (int * int) list) =
  (x > 0 && List.mem (x - 1, y) full)
  || (x < 7 && List.mem (x + 1, y) full)
  || (y > 0 && List.mem (x, y - 1) full)
  || (y < 8 && List.mem (x, y + 1) full)

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
    if
      List.mem (x, y) full
      || List.mem (adjacent t (x, y)) full
      || full_neighbors (x, y) full
      || full_neighbors (adjacent t (x, y)) full
    then place_aux (t, full)
    else (new_coords t (x, y), (x, y) :: adjacent t (x, y) :: full)
  in
  match t_lst with
  | [] -> []
  | h :: t ->
      let pair = place_aux (h, full) in
      get_tile pair :: place t (get_full pair)

let tile_list =
  let s1 = strings double_vowels [] in
  let t1 = create s1 [] in
  place t1 []