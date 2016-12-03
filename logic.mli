open Data
open Gmap

(* [a >:: lst] is the updated list after inserting [a] to [lst] if [a] is not 
 * already in [lst]. *)
val (>::) : 'a -> 'a list -> 'a list

(* [assign_was_moved s p b] assigns bool [b] to the [was_moved] field of
 * whoever playing the character of prof [p] in state [s]. If no one plays that
 * character, then [s] is simply unchanged. *)
val assign_was_moved : state -> prof -> bool -> state

(* [roll_two_dice ()] simulates rolling two dice, prints the results, and 
 * returns the sum. *)
val roll_two_dice : unit -> int

(* [get_choice ()] is [1] if the user selects the first choice and [2] if
 * the user selects the second choice. *)
val get_choice_two : unit -> int

(* [get_choice_three ()] is [1] if the user selects the first choice, [2] if
 * the user selects the second choice, and [3] if the third. *)
val get_choice_three : unit -> int

(* [get_choice_four ()] is [1] if the user selects the first choice, [2] if
 * the second, [3] if the third, and [4] if the fourth. *)
val get_choice_four : unit -> int

(* [get_choice_num_ai ()] is [2] up till [5] if the user chooses to play with 
 * [2] to [5] AI's. *)
val get_choice_num_ai : unit -> int

(* TODO [ai_disprove ai lst guess] figures out which card to reveal in response
 * to a suggestion [guess].
 * Returns: [Some c] where [c] is a card that [ai] can reveal. Or, if [ai] has
 * none of the cards in [guess], then it will return [None].
 *)
val ai_disprove : ai -> case_file -> ai list -> card option

(* TODO [user_disprove s lst guess] is [None] if the user does not have any card
 * to disprove the suggestion [guess] and a card option if the user has the 
 * card(s) and wishes to disprove [guess] with that card. *)
val user_disprove : state -> case_file -> ai list -> card option

