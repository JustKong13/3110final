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
    test "initalized game time" init_game_state.time_elapsed 0.0;
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
  ]

let double_vowels = "aabcdeefghiijklmnoopqrstuuvwxyz"

(*let strings_test (name : string) (a_string : string) (acc : string list)
  (expected_output : string list) : test = name >:: fun _ -> assert_equal
  expected_output (strings a_string acc) ~printer:(pp_list pp_string) *)

(*let strings_tests = [ strings_test "tests word list generator" double_vowels
  [] [] ]*)

(*Tile.ml tests*)
(*let str_list = [ "dm"; "s"; "e"; "a"; "ng"; "bn"; "k"; "lj"; "en"; "ie" ];;
  let t1 = create str_list [];; place t1 [];; *)

(*let create_test (name : string) (input : string list) (expected_output :
  Tile.t list) : test = name >:: fun _ -> assert_equal expected_output (create
  str_list [])

  let tile_tests = [ create_test "create tiles" str_list [] ]*)

let tests =
  "wordbite test suite"
  >::: List.flatten [ word_validator_tests; wordbite_test ]

let _ = run_test_tt_main tests
