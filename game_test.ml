open OUnit2
open Main
open Ai
open Data


let ai1 ={
  character ="White";
  hand=[];
  was_moved=false;
  is_in_game=true;
  difficulty=Easy;
  destination=None;
  known_cards=[];
  possible_cards=[];
  past_guesses=[]
  (*(case_file * prof * (prof option)) list;*)
}

let ai_tests = [

"init"  >:: (fun _ -> assert_equal (ai1)
    (init "White" Easy []));

]

let ai_test_suite =
  "Clue Tests"
  >::: tests


let _ = run_test_tt_main test_suite
