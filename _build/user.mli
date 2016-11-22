open Data
open Gmap

(* [move s] takes in a state [s] and outputs the new state that the user
 * will be in after s/he makes the move. *)
val move: state -> state

(* [suggestion h] takes in a case that the user wants to suggest to the 
 * group. This then is passed around to the other players in clockwise 
 * fashion and each other player must attempt to disprove it. If no player 
 * can disprove it, this function will return None. If a player can, then 
 * s/he reveals the card to the user in order to disprove it and the 
 * function will return Some card. *)
val suggestion: case_file -> card option

(* [endturn s] takes in the current state and outputs a state when the current 
 * player has changed. *)
val endturn: state -> state

(* [accusation h] takes in a hand and compares it to the culprits. If they are
 * the same, then returns true. Else, it returns false. *)
val accusation: case_file -> bool

(* [secret_possage s] is only possible when the user is on a square where it 
 * is possible to move to a secret passage. Takes the state that the game was 
 * in orginally and returns a state where the user has been moved to the 
 * other end of the secret passage. *)
val secret_passage: state -> state

(* [quit s] takes current state and returns a state where the player has quit.
 * That state should also tell the player the true culprits. *)
val quit: state -> state

(* [disprove h d] takes in a hand and a deck. h is the suggestion and d is the
 * deck of the player that is trying to disprove it. If the hand cannot be 
 * disproved, this function returns None. If it can, it returns the disproving 
 * card - Some card. *)
val disprove: case_file -> hand -> card option
