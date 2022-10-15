(** IN ORDER TO RUN TESTS, YOU MUST DO [dune.exec ./test/main.exe] IN THE
    TERMINAL*)

open OUnit2
open Wordbite

let available_strs_test (name: string) (alphabet_string : string) (expected_output : string list)
let tests = "wordbite test suite" >::: []
let _ = run_test_tt_main tests
