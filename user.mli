open Data
open Gmap

(* [user_turn s] is the new state after the user finishes his/her turn when
 * the current state is [s]. *)
val user_turn: state -> state

(* [user_disprove s case_file] is the new state after the user disproves the  
 * current suggestion (if possible). *)
val user_disprove: state -> case_file -> state
