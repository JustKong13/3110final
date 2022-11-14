(* This file was referenced using FA22 CS 3110 A2 assignment. *)

open Game.Wordbite
open Game.Wordvalidator

exception InvalidString

(** [sort s] sorts string [s] in alphabetical order. *)
let sort s =
  let n = String.length s in
  let a = Array.init n (fun i -> s.[i]) in
  Array.sort Char.compare a;
  String.init n (fun i -> a.(i))

(** [contains s1 s2] checks whether string [s2] is a substring of string [s1]. *)
let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false

(** [form_word s words acc] iterates over string list [words] and checks whether
    each string is a substring of string [s]. Returns a string of all of the
    elements in [words] that are substrings in [s]. *)
let rec form_word s words acc =
  match words with
  | [] -> acc
  | h :: t -> if contains s h then form_word s t acc ^ h else form_word s t acc

(** [print_lst words] prints [lst] in a readable format. *)
let rec print_lst words =
  match words with
  | [] -> ""
  | [ h ] -> h
  | h :: t -> h ^ ", " ^ print_lst t

(** [parse_input s words] parses [s] by checking if it is constructed by
    concatenation of strings in list [words] and checks whether [s] is a real
    word, otherwise prompts the user for another input. *)
let rec parse_input s words =
  try
    if
      check_word s
      && (String.equal (sort (form_word s words "")) (sort s)
         || contains (sort (form_word s words "")) (sort s)
         || contains (form_word s words "") s)
    then (
      ANSITerminal.print_string [ ANSITerminal.green ] ("Word valid! " ^ s);
      print_endline "\nType another word!\n";
      print_string "> ";
      parse_input (read_line ()) words)
    else raise InvalidString
  with InvalidString ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\n\nYou entered:" ^ s
     ^ "\n\
        Unfortunately, it does not consist of strings in the list or is not a \
        real word.\n");
    print_string "[";
    print_string (print_lst words);
    print_endline "]";
    print_endline
      "\nType in a word constructed by the letters in the list above\n";
    print_string "> ";
    parse_input (read_line ()) words

(** [play_game input] starts the Wordbite game if [input] is ["start"],
    otherwise prompts the user to correctly enter ["start"] again. *)
let rec play_game input =
  try
    match input with
    | "start" ->
        ANSITerminal.print_string [ ANSITerminal.green ]
          "\n\nThe game has started!\n";
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\n\nThe list of usable letters are:\n";
        (* Add helper function call to wordbite.ml *)
        let words = strings "aabcdeefghiijklmnoopqrstuuvwxyz" [] in
        print_string "[ ";
        print_string (print_lst words);
        print_endline " ]";
        print_endline
          "\nType in a word constructed by the letters in the list above\n";
        print_string "> ";
        parse_input (read_line ()) words
    | _ -> raise InvalidString
  with InvalidString ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\nYou did not type in \'start\' correctly...\n";
    print_endline "Please type \'start\' to start the game.\n";
    print_string "> ";
    play_game (read_line ())

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "\n\nWelcome to Wordbite.\n";
  print_endline "Please type \'start\' to start the game.\n";
  print_string "> ";
  play_game (read_line ())

(* Execute the game engine. *)
let () = main ()