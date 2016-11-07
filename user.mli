import Game



(*[move s] takes in a state s and outputs the new state that the player
will be in after s/he makes the move. The state will include the map, location,
etc.

TODO: implement*)
  val move: state -> state

(*[suggestion h] takes in a hand that the player wants to suggest to the group.
* This then is passed around to the other players in clockwise fashion and each
* other player must attempt to disprove it. If no players can disprove it, this
* function will return None. If a player can, then they reveal the card they possess
* to the player in order to disprove it and the function will return Some card.

* TODO: implement*)
  val suggestion: case -> option

(*[endturn s] takes the currents state andoutputs a state when the current player
* has changed.*)
  val endturn: state -> state

(*[accusation h] takes in a hand and compares it to the culprits. If they are
* the same, then returns true. Else, it returns false.

TODO: implement.*)
  val accusation: case -> bool

(*[secretpossage s] Only possible when the player is on a square where it is
* possible to move to a secret passage. Takes the state that the game was in orginally
* and returns a state where the player has been moved to the other end of the
* secret passage.

TODO: implement*)
  val secretpassage: state-> state

(*[quit s] takes current state and returns a state where the player has quit.
* That state should also tell the player the true culprits.

TODO: implement.*)
  val quit: state -> state

(*[disprove h d] takes in a hand and a deck. h is the suggestion and d is the
* deck of the player that is trying to disprove it. If the hand cannot be disproved,
* this function returns None. If it can, it returns the disproving card - Some card.

TODO: implement*)
  val disprove: case -> hand -> card option


