(** IN ORDER TO RUN TESTS, YOU MUST DO [dune exec test/main.exe] IN THE TERMINAL*)

open OUnit2
open Wordbite

let alphabet_double_vowels = "aabcdeefghiijklmnoopqrstuuvwxyz"

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

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

let available_strs_test (name : string) (alphabet_string : string)
    (accumulated_lst : string list) (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (available_strs alphabet_string accumulated_lst)
    ~printer:(pp_list pp_string)

let tests =
  "wordbite test suite"
  >::: [ available_strs_test "1" alphabet_double_vowels [] [ "a" ] ]

let _ = run_test_tt_main tests
