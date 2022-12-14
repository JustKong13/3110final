(** TESTING PLAN: Since our project was a game, that means that a lot of our
    actual testing came through user testing. However, that is difficult to test
    for in a test suite, so a lot of our testing in this file was localized
    testing (i.e. testing backend functions). We manually tested the gui portion
    of the project by opening the graphics window every time and testing whether
    the components are drawn correctly or in their correct coordinates. In
    addition, there was user input in the GUI, so we had to test if the user
    input actually had a response by drawing a string which is similar to
    debugging by printing statements. We tested backend functionality for or
    game by testing the helpers that we wrote in our modules, and we did this by
    creating arbitrary game states which we can predict the behavior of. Most of
    the test cases are developed with black box testing since most of our helper
    functions involve randomized game states. Thus, we tested the Board, Tile,
    Wordvalidator, and Wordbite modules with mutable game states and made
    changes to the game state through manipulation and mutability in order to
    test our project. Our game states are generated randomly, which is why we
    needed to create our own game state. Also, since our game state was mutable,
    it was difficult to test whether the functions that mutate game state were
    actually doing the job without ruining other test cases. The way we got
    around this was by writing complementary functions that returned the new
    game state rather than mutate the game state. These functions were meant
    primarally to test functionality, and were ultimately omitted from the
    actual game in favor of functions that mutated game state. Through thorough
    testing of the GUI and backend functions, we are able to pass all the tests
    and our game system works as intended with the specificatins of the modules
    and our intended idea and purpose for this game.*)

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
(* These tests are to ensure our word validator was correct, as well as test
   basic functionality with creating boards*)
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
    test "updating the score given list of empty found words" (update_score [])
      0;
    test "update score given list of words" (update_score [ "laugh" ]) 800;
    test "update score given long list of words"
      (update_score [ "laugh"; "tongue"; "top" ])
      2300;
  ]

let double_vowels = "aabcdeefghiijklmnoopqrstuuvwxyz"

open Tile

let list_of_tiles =
  [
    { tstring = "a"; ttype = ATile; position = (0, 0) };
    { tstring = "sa"; ttype = HTile; position = (6, 7) };
    { tstring = "sa"; ttype = VTile; position = (5, 5) };
  ]

let tile_list_1 =
  [
    { tstring = "g"; ttype = ATile; position = (3, 8) };
    { tstring = "o"; ttype = ATile; position = (6, 4) };
    { tstring = "uo"; ttype = VTile; position = (2, 4) };
    { tstring = "it"; ttype = HTile; position = (3, 3) };
    { tstring = "ap"; ttype = VTile; position = (5, 5) };
    { tstring = "i"; ttype = ATile; position = (2, 7) };
    { tstring = "c"; ttype = ATile; position = (0, 8) };
    { tstring = "y"; ttype = ATile; position = (7, 3) };
    { tstring = "mi"; ttype = HTile; position = (0, 2) };
    { tstring = "ao"; ttype = HTile; position = (6, 8) };
  ]

let list_of_tiles1_board =
  [
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "m"; "i"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "i"; "t"; "-"; "-"; "y" ];
    [ "-"; "-"; "u"; "-"; "-"; "-"; "o"; "-" ];
    [ "-"; "-"; "o"; "-"; "-"; "a"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "p"; "-"; "-" ];
    [ "-"; "-"; "i"; "-"; "-"; "-"; "-"; "-" ];
    [ "c"; "-"; "-"; "g"; "-"; "-"; "a"; "o" ];
  ]

let game_state1 =
  {
    score = 0;
    words_found = [];
    board = generate_game_board tile_list_1 B.empty;
    tile_list = tile_list_1;
  }

let list_of_tiles2_board =
  [
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "c"; "a"; "n"; "t"; "-"; "y"; "e"; "s" ];
    [ "-"; "-"; "-"; "-"; "t"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "o"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "n"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
  ]

let tile_list_2 =
  [
    { tstring = "ca"; ttype = HTile; position = (0, 2) };
    { tstring = "nt"; ttype = HTile; position = (2, 2) };
    { tstring = "y"; ttype = ATile; position = (5, 2) };
    { tstring = "es"; ttype = HTile; position = (6, 2) };
    { tstring = "t"; ttype = ATile; position = (4, 3) };
    { tstring = "on"; ttype = VTile; position = (4, 4) };
  ]

let game_state2 =
  {
    score = 0;
    words_found = [];
    tile_list = tile_list_2;
    board = generate_game_board tile_list_2 B.empty;
  }

let tile_tests =
  [
    test "finding tile loc 1"
      (find_tile (0, 0) list_of_tiles)
      ({ tstring = "a"; ttype = ATile; position = (0, 0) }, true);
    test "finding tile loc 2"
      (find_tile (6, 7) list_of_tiles)
      ({ tstring = "sa"; ttype = HTile; position = (6, 7) }, true);
    test "finding adjacent tile of HTile"
      (find_tile (7, 7) list_of_tiles)
      ({ tstring = "sa"; ttype = HTile; position = (6, 7) }, false);
    test "finding adjacent tile of VTile"
      (find_tile (5, 6) list_of_tiles)
      ({ tstring = "sa"; ttype = VTile; position = (5, 5) }, false);
    test_exception "finding adjacent tile of HTile" Game.Wordbite.TileNotFound
      (fun _ -> find_tile (7, 8) list_of_tiles);
    test "testing move 1"
      (move (0, 0) (2, 2) list_of_tiles)
      [
        { tstring = "a"; ttype = ATile; position = (2, 2) };
        { tstring = "sa"; ttype = HTile; position = (6, 7) };
        { tstring = "sa"; ttype = VTile; position = (5, 5) };
      ];
    test_exception "moving to overlap" Game.Wordbite.TileOverlap (fun _ ->
        move (0, 0) (6, 7) list_of_tiles);
    test_exception "moving to overlap adjacent" Game.Wordbite.TileOverlap
      (fun _ -> move (0, 0) (7, 7) list_of_tiles);
    test "moving next to a tile valid"
      (move (0, 0) (7, 8) list_of_tiles)
      [
        { tstring = "a"; ttype = ATile; position = (7, 8) };
        { tstring = "sa"; ttype = HTile; position = (6, 7) };
        { tstring = "sa"; ttype = VTile; position = (5, 5) };
      ];
    test_exception "moving and then checking location of the original position"
      Game.Wordbite.TileNotFound (fun _ ->
        find_tile (0, 0) (move (0, 0) (2, 2) list_of_tiles));
    test "generate empty board"
      (B.board_to_list (generate_game_board [] B.empty))
      (B.board_to_list B.empty);
    test "generate correct board given tile list"
      (B.board_to_list (generate_game_board list_of_tiles B.empty))
      [
        [ "a"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "s"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "a"; "-"; "-" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "s"; "a" ];
        [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
      ];
    test "generate correct board given tile list randomized"
      (B.board_to_list (generate_game_board tile_list_1 B.empty))
      list_of_tiles1_board;
    test_exception "moving ATile out of bounds" Game.Wordbite.OutOfBound
      (fun _ -> move (0, 0) (9, 9) list_of_tiles);
    test_exception "moving HTile out of bounds" Game.Wordbite.OutOfBound
      (fun _ -> move (6, 7) (8, 8) list_of_tiles);
    test_exception "moving HTile out of bounds other tile"
      Game.Wordbite.OutOfBound (fun _ -> move (7, 7) (0, 1) list_of_tiles);
    test_exception "moving VTile out of bounds" Game.Wordbite.OutOfBound
      (fun _ -> move (5, 5) (5, 8) list_of_tiles);
    test_exception "moving VTile out of bounds other tile"
      Game.Wordbite.OutOfBound (fun _ -> move (5, 6) (5, 0) list_of_tiles);
    test "checking board for no valid words 1"
      (check_for_words (0, 0) game_state1)
      [];
    test "checking board for no valid words 1"
      (check_for_words (7, 2) game_state1)
      [];
  ]

let find_words_of_row (lst : string list) =
  lst |> create_string_of_row |> generate_list_of_words |> get_valid_words

(* creates a board with the word ban horizontally and not vertically*)
let board1_rep =
  B.empty
  |> B.place_letter "b" (1, 1)
  |> B.place_letter "a" (1, 2)
  |> B.place_letter "n" (1, 3)
  |> B.place_letter "o" (2, 3)
  |> B.place_letter "t" (3, 3)

(** to test game board word search function*)
let game_state_board1 : game =
  { score = 0; words_found = []; board = board1_rep; tile_list = [] }

let word_finder_test =
  [
    test "find word banana"
      (find_words_of_row [ "-"; "b"; "a"; "n"; "a"; "n"; "a"; "-" ])
      [ "banana" ];
    test "find word but and and"
      (find_words_of_row [ "-"; "a"; "n"; "d"; "-"; "b"; "u"; "t" ])
      [ "and"; "but" ];
    test "does not find invalid word in row"
      (find_words_of_row [ "b"; "a"; "s" ])
      [];
    test "finds valid word and omits invalid word 1"
      (find_words_of_row [ "y"; "e"; "s"; "-"; "c"; "b"; "t" ])
      [ "yes" ];
    test "finds valid word and omits invalid word 2"
      (find_words_of_row [ "c"; "b"; "t"; "-"; "y"; "e"; "s" ])
      [ "yes" ];
    test "does not find word thats attached to extraneous letters"
      (find_words_of_row [ "c"; "y"; "e"; "s" ])
      [];
    test "finds word in row and column"
      (check_for_words (1, 3) game_state_board1)
      [ "not"; "ban" ];
    test "finds word in column"
      (check_for_words (1, 1) game_state_board1)
      [ "ban" ];
    test "finds word in row"
      (check_for_words (7, 3) game_state_board1)
      [ "not" ];
    test_exception "moving a nonexistant tile" TileNotFound (fun _ ->
        move_on_board (0, 0) (1, 1) B.empty);
  ]

let tests =
  "wordbite test suite"
  >::: List.flatten
         [ word_validator_tests; wordbite_test; tile_tests; word_finder_test ]

let _ = run_test_tt_main tests
