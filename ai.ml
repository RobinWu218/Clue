open Data
open Map

(* [t] is the data type representing the Ai's state and goals *)
type t = unit

(***********************************************
 * Utility methods
 ***********************************************)

(* [init p d hand] creates and returns an AI data structure that represents an
 * AI playing character [p] on difficulty [d], with starting hand [hand].
 * Returns: the initialized setup for an AI.
 *)
let init p d hand =
  failwith "unimplemented"

(* [get_ai state p]
 * Returns: the AI data structure for the AI playing character [p]
 *)
let get_ai state p =
  failwith "unimplemented"

(* [get_difficulty ai]
 * Returns: the difficulty of the AI [ai].
 *)
let get_difficulty ai =
  failwith "unimplemented"

(* [still_in_game ai]
 * Returns: [true] iff the AI [ai] is still in the game. (If you're out you
 * can still prove suggestions wrong)
 *)
let still_in_game ai =
  failwith "unimplemented"


(************************************************
 * Methods for interacting with game state
 ************************************************)

(* [update_ai ai player guess player2] updates the knowledge of [ai] when 
 * [player] makes a [guess] that got disproved by [player2]. *)
let update_ai ai player guess player2 =
  failwith "unimplemented"

let make_suggestion ai =
  failwith "unimplemented"

let make_accusation ai =
  failwith "unimplemented"


(* [step state p] peforms a turn for player [p] (an AI player). This involves:
 *   - defining and setting goals by processing knowledge from suggestions and
 *     making deductions about other players' turns.
 *   - moving the AI around the map
 *   - forming and making suggestions/accusations
 * Returns: an updated game state.
 *)

let step state p =
  failwith "unimplemented"

(* [reveal_card ai guess] figures out which card to reveal in response
 * to a suggestion [guess].
 * Returns: [Some c] where [c] is a card that [ai] can reveal. Or, if [ai] has
 * none of the cards in [guess], then it will return [None].
 *)
let reveal_card ai guess =
  failwith "unimplemented"