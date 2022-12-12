(** IN ORDER TO RUN TESTS, YOU MUST DO
    [dune.exec ./test/main.exe (mac) dune exec test/main.exe (other)] IN THE
    TERMINAL*)

open OUnit2
open Game
open Wordvalidator
open Board
open Wordbite

let pp_string s = "\"" ^ s ^ "\""
(*let pp_tile t = tile_string t*)

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let test name f expeceted_output =
  name >:: fun _ -> assert_equal f expeceted_output

let test_exception name expected_exception exp =
  name >:: fun _ -> assert_raises expected_exception exp

module B = Board

let b1 = B.empty

(*Word Validator and Board Tests*)

let word_validator_tests =
  [
    test "banana is a real word" (check_word "banana") true;
    test "frog is a real word" (check_word "frog") true;
    test "asdf is not a real word" (check_word "asdf") false;
    test "dont is a real word (contraction word)" (check_word "dont") true;
    test "empty board" (B.board_to_list b1)
      [
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
      ];
    test "inserting a into (0,0)"
      (B.board_to_list (B.place_letter "a" (0, 0) b1))
      [
        [ "a"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
      ];
    test "inserting a into (4,4)"
      (B.board_to_list (B.place_letter "a" (4, 4) b1))
      [
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "a"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
      ];
    test "finding a" (B.get_letter (4, 4) (B.place_letter "a" (4, 4) b1)) "a";
    test "finding an empty tile"
      (B.get_letter (4, 4) (B.place_letter "a" (5, 4) b1))
      "-";
    test "is_empty on a non empty tile"
      (B.is_empty (4, 4) (B.place_letter "a" (4, 4) b1))
      false;
  ]

(*Wordbite.ml tests*)

module W1 = Wordbite
module W2 = Wordbite

let init_game_state = W1.init_game
let test_board = B.place_letter "a" (4, 4) b1
let test1_game_state = W2.init_game
let one_point = W2.update_game_state "asdf" test1_game_state
let two_point = W2.update_game_state "abcd" one_point

let wordbite_test =
  [
    test "initalized game score" init_game_state.score 0;
    test "initalized words found" init_game_state.words_found [];
    test "move tile on board"
      (B.board_to_list
         (move_on_board (4, 4) (1, 1) (B.place_letter "a" (4, 4) b1)))
      [
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "a"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
      ];
    test "testing game score 1 words found" one_point.score 1;
    test "testing game state 1 words found" one_point.words_found [ "asdf" ];
    test "testing game score 2 words found" two_point.score 2;
    test "testing game state 2 words found" two_point.words_found
      [ "abcd"; "asdf" ];
    test "transforming row to string"
      (create_string_of_row [ "-"; "-"; "-"; "a"; "-"; "a"; "b"; "c" ])
      "---a-abc";
    test "transforming empty row to string"
      (create_string_of_row [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ])
      "--------";
  ]

let double_vowels = "aabcdeefghiijklmnoopqrstuuvwxyz"

open Tile

let list_of_tiles =
  [
    { tstring = "a"; ttype = ATile; position = (0, 0) };
    { tstring = "sa"; ttype = HTile; position = (7, 7) };
    { tstring = "sa"; ttype = VTile; position = (5, 5) };
  ]

let tile_tests =
  [
    test "finding tile loc 1"
      (find_tile (0, 0) list_of_tiles)
      ({ tstring = "a"; ttype = ATile; position = (0, 0) }, true);
    test "finding tile loc 2"
      (find_tile (7, 7) list_of_tiles)
      ({ tstring = "sa"; ttype = HTile; position = (7, 7) }, true);
    test "finding adjacent tile of HTile"
      (find_tile (8, 7) list_of_tiles)
      ({ tstring = "sa"; ttype = HTile; position = (7, 7) }, false);
    test "finding adjacent tile of VTile"
      (find_tile (5, 6) list_of_tiles)
      ({ tstring = "sa"; ttype = VTile; position = (5, 5) }, false);
    test_exception "finding adjacent tile of HTile" (Failure "Tile not found")
      (fun _ -> find_tile (7, 8) list_of_tiles);
    test "testing move 1"
      (move (0, 0) (2, 2) list_of_tiles)
      [
        { tstring = "a"; ttype = ATile; position = (2, 2) };
        { tstring = "sa"; ttype = HTile; position = (7, 7) };
        { tstring = "sa"; ttype = VTile; position = (5, 5) };
      ];
    test_exception "moving to overlap"
      (Failure "Cannot move this tile due to overlapping.") (fun _ ->
        move (0, 0) (7, 7) list_of_tiles);
    test_exception "moving to overlap adjacent"
      (Failure "Cannot move this tile due to overlapping.") (fun _ ->
        move (0, 0) (8, 7) list_of_tiles);
    test "moving next to a tile valid"
      (move (0, 0) (7, 8) list_of_tiles)
      [
        { tstring = "a"; ttype = ATile; position = (7, 8) };
        { tstring = "sa"; ttype = HTile; position = (7, 7) };
        { tstring = "sa"; ttype = VTile; position = (5, 5) };
      ];
    test_exception "moving and then checking location of the original position"
      (Failure "Tile not found") (fun _ ->
        find_tile (0, 0) (move (0, 0) (2, 2) list_of_tiles));
  ]

let tests =
  "wordbite test suite"
  >::: List.flatten [ word_validator_tests; wordbite_test; tile_tests ]

let _ = run_test_tt_main tests
