(** IN ORDER TO RUN TESTS, YOU MUST DO [dune.exec ./test/main.exe] IN THE 
    TERMINAL*)

open OUnit2
open Wordbite
open Wordvalidator

let check_word_test (name : string) (word : string) (expected_output : bool) :
    test =
  name >:: fun _ -> assert_equal expected_output (check_word word)

let check_word_tests =
  [ check_word_test "asdf is not a valid word" "asdf" false ]

let tests = "wordbite test suite" >::: [ check_word_tests ]
let _ = run_test_tt_main tests