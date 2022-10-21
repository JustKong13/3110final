(** IN ORDER TO RUN TESTS, YOU MUST DO [dune.exec ./test/main.exe] IN THE
    TERMINAL*)

open OUnit2
open Wordbite

let word_validator_test name word expecetd_output =
  name >:: fun _ -> assert_equal (check_word word) expecetd_output

let word_validator_tests =
  [
    word_validator_test "banana is a real word" "banana" true;
    word_validator_test "frog is a real word" "frog" true;
    word_validator_test "asdf is not a real word" "asdf" false;
    word_validator_test "dont is a real word (contraction word)" "dont" true;
  ]

let tests = "wordbite test suite" >::: List.flatten [ word_validator_tests ]
let _ = run_test_tt_main tests
