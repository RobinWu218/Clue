open Data
open Gmap

(* [user_turn s] is the new state after the user finishes his/her turn when
 * the current state is [s]. *)
val user_turn: state -> state
