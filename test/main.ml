(** IN ORDER TO RUN TESTS, YOU MUST DO [dune.exec ./test/main.exe] IN THE
    TERMINAL*)

open OUnit2
open Wordbite

let tests = "wordbite test suite" >::: []
let _ = run_test_tt_main tests