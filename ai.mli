open Data
open Gmap

(* [t] is the data type representing the Ai's state and goals *)
type t

(***********************************************
 * Utility methods
 ***********************************************)

(* [init p d hand] creates and returns an AI data structure that represents an
 * AI playing character [p] on difficulty [d], with starting hand [hand].
 * Returns: the initialized setup for an AI.
 *)
val init: string -> difficulty -> hand -> t

(* [get_ai state p]
 * Returns: the AI data structure for the AI playing character [p]
 *)
val get_ai: state -> string -> t

(* [get_difficulty ai]
 * Returns: the difficulty of the AI [ai].
 *)
val get_difficulty: t -> difficulty

(* [still_in_game ai]
 * Returns: [true] iff the AI [ai] is still in the game. (If you're out you
 * can still prove suggestions wrong)
 *)
val still_in_game: t -> bool


(************************************************
 * Methods for interacting with game state
 ************************************************)

 (* [update_ai ai player guess player2] updates the knowledge of [ai] when
 * [player] makes a [guess] that got disproved by [player2]. *)
 val update_ai: t -> prof -> case_file -> prof -> t

(* [ai_step state ai] peforms a turn for [ai]. This involves:
 *   - defining and setting goals by processing knowledge from suggestions and
 *     making deductions about other players' turns.
 *   - moving the AI around the map
 *   - forming and making suggestions/accusations
 * Returns: an updated game state.
 *)
val ai_step: state -> t -> state

(* [ai_disprove ai guess] figures out which card to reveal in response
 * to a suggestion [guess].
 * Returns: [Some c] where [c] is a card that [ai] can reveal. Or, if [ai] has
 * none of the cards in [guess], then it will return [None].
 *)
val ai_disprove: t -> card list -> card option