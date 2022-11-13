(** IN ORDER TO RUN TESTS, YOU MUST DO [dune.exec ./test/main.exe] IN THE
    TERMINAL*)

open OUnit2
<<<<<<< HEAD
open Game.Wordbite
open Game.Wordvalidator
=======
open Game
open Wordvalidator
open Board
>>>>>>> ac12cbf21169cc6f053624038cea60c09f6b42da

let test name f expeceted_output =
  name >:: fun _ -> assert_equal f expeceted_output

module B = Board

let b1 = B.empty

let word_validator_tests =
  [
    test "banana is a real word" (check_word "banana") true;
    test "frog is a real word" (check_word "frog") true;
    test "asdf is not a real word" (check_word "asdf") false;
    test "dont is a real word (contraction word)" (check_word "dont") true;
    test "empty board" (B.board_to_list b1)
      [
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
      ];
    test "inserting a into (0,0)"
      (B.board_to_list (B.place_letter 'a' (0, 0) b1))
      [
        [ 'a'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
      ];
    test "inserting a into (4,4)"
      (B.board_to_list (B.place_letter 'a' (4, 4) b1))
      [
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; 'a'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
        [ '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-' ];
      ];
    test "finding a" (B.get_letter (4, 4) (B.place_letter 'a' (4, 4) b1)) 'a';
    test "finding an empty tile"
      (B.get_letter (4, 4) (B.place_letter 'a' (5, 4) b1))
      '-';
  ]

let tests = "wordbite test suite" >::: List.flatten [ word_validator_tests ]
let _ = run_test_tt_main tests
