open Data
open Gmap

(* [init p h d lst] is the AI data structure that represents an AI playing
 * character [p] on difficulty level [d], with hand [h]. [lst] is a list of
 * all players initialized in the game.
 *)
val init: prof -> hand -> difficulty -> prof list -> ai

(*[accuse a s] produces a state where the accusation has been made
 * with the case_file that the ai believes is correct. Does not depend on ai
 * difficulty. The ai only makes an accusation
 * when it has narrowed down the possible cards to 3.
*)
val accuse: ai-> state-> bool * state

(* [disprove_case p n guess s] checks if the professor corresponding
 * to integer [n] is represented by any ai and if so, if that ai can disprove
 * [guess], before possibly calling [disprove_loop (n+1) guess s] to move
 * on to check the next professor.
*)
val disprove_case: prof-> int-> int-> case_file-> state->((prof * card) option)

(*[suggest a s] returns a state after deciding whether or not to suggest and
 * doing the suggesting if that is what it has decided.
 * AI logic easy: random.
 * AI logic medium and hard: only guesses possible cards, not the ones that
 * it knows exists.
*)
val suggest: ai-> state-> state

(* [move a n bop s] allows the AI [a]'s character to move n steps,
 * or fewer if the character gets into a building
 * before using up all the steps. If [bop] is [Some b'] then user cannot
 * move into [b'] since s/he just left that building in the same turn.
*)
val move: ai-> int -> building option-> state -> state

(* [step ai state] peforms a turn for [ai]. This involves:
 *   - defining and setting goals by processing knowledge from suggestions and
 *     making deductions about other players' turns.
 *   - moving the AI around the map
 *   - forming and making suggestions/accusations
 * Returns: an updated game state.
 *)
val step: ai -> state -> state

