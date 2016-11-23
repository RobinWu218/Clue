open Data
open Gmap

let parse_card (s:string) : card =
  failwith "TODO"

(* [move d s] takes in a state [s] and outputs the new state that the user
 * will be in after s/he moves the specified steps in the specified direction
 * in [d] if possible. The user stops at wherever s/he is blocked without
 * producing an error. *)
let rec move (d:dir) (s:state) : state =
  failwith "TODO"

(* [suggestion h] takes in a case that the user wants to suggest to the 
 * group. This then is passed around to the other players in clockwise 
 * fashion and each other player must attempt to disprove it. If no player 
 * can disprove it, this function will return None. If a player can, then 
 * s/he reveals the card to the user in order to disprove it and the 
 * function will return Some card. *)
let suggestion : case_file -> option =
  failwith "TODO"

(* [endturn s] takes in the current state and outputs a state when the current 
 * player has changed. *)
let endturn : state -> state =
  failwith "TODO"

(* [accusation h] takes in a hand and compares it to the culprits. If they are
 * the same, then returns true. Else, it returns false. *)
let accusation : case_file -> bool =
  failwith "TODO"

(* [secret_possage s] is only possible when the user is on a square where it 
 * is possible to move to a secret passage. Takes the state that the game was 
 * in orginally and returns a state where the user has been moved to the 
 * other end of the secret passage. *)
let secret_passage : state -> state =
  failwith "TODO"

(* [quit s] takes current state and returns a state where the player has quit.
 * That state should also tell the player the true culprits. *)
let quit (s:state) : state =
  failwith "TODO"

(* [disprove h d] takes in a case and a deck. h is the suggestion and d is the
 * deck of the player that is trying to disprove it. If the hand cannot be 
 * disproved, this function returns None. If it can, it returns the disproving 
 * card - Some card. *)
let disprove (h:case_file) (d:hand) : card option =
  failwith "TODO"


