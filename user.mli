open Data
open Gmap

(* [user_turn s] is the new state after the user finishes his/her turn when
 * the current state is [s]. *)
val user_turn: state -> state

(* [user_disprove s case_file] is [None] if the user does not have any card
 * to disprove the suggestion [guess] and a card option if the user has the 
 * card(s) and wishes to disprove [guess] with that card. *)
val user_disprove: state -> case_file -> card option
