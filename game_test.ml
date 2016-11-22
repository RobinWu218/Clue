open OUnit2
open Main

let tests = []

let test_suite = 
  "Clue Tests"
  >::: tests


let _ = run_test_tt_main test_suite