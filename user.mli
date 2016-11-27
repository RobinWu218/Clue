open Data
open Gmap

(* [user_step s] is the new state after the user finishes his/her turn when
 * the current state is [s]. *)
val user_step: state -> state

(* [user_disprove s guess] is [None] if the user does not have any card
 * to disprove the suggestion [guess] and a card option if the user has the 
 * card(s) and wishes to disprove [guess] with that card. *)
val user_disprove: state -> case_file -> card option
