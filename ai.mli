open Data
open Gmap

(***********************************************
 * Utility methods
 ***********************************************)

(* [init p d hand] creates and returns an AI data structure that represents an
 * AI playing character [p] on difficulty [d], with starting hand [hand].
 * Returns: the initialized setup for an AI.
 *)
val init: string -> difficulty -> hand -> ai

(* [get_ai state p]
 * Returns: the AI data structure for the AI playing character [p]
 *)
val get_ai: state -> string -> ai

(* [get_difficulty ai]
 * Returns: the difficulty of the AI [ai].
 *)
val get_difficulty: ai -> difficulty

(* [still_in_game ai]
 * Returns: [true] iff the AI [ai] is still in the game. (If you're out you
 * can still prove suggestions wrong)
 *)
val still_in_game: ai -> bool


(************************************************
 * Methods for interacting with game state
 ************************************************)

 (* [update_ai ai player guess player2] updates the knowledge of [ai] when
 * [player] makes a [guess] that got disproved by [player2]. *)
 val update_ai: ai -> prof -> case_file -> prof -> ai


(* [ai_step state ai] peforms a turn for [ai]. This involves:
 *   - defining and setting goals by processing knowledge from suggestions and
 *     making deductions about other players' turns.
 *   - moving the AI around the map
 *   - forming and making suggestions/accusations
 * Returns: an updated game state.
 *)
val ai_step: state -> ai -> state

(* [ai_disprove ai guess] figures out which card to reveal in response
 * to a suggestion [guess].
 * Returns: [Some c] where [c] is a card that [ai] can reveal. Or, if [ai] has
 * none of the cards in [guess], then it will return [None].
 *)
val ai_disprove: ai -> card list -> card option
