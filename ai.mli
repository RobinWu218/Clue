open Data
open Gmap

(*******************)
(* utility methods *)
(*******************)

(* [init p h d] is the AI data structure that represents an AI playing
 * character [p] on difficulty level [d], with hand [h].
 *)
val init: prof -> hand -> difficulty -> ai

(* [get_ai p s] is the AI data structure for the AI playing character [p] in
 * state [s].
 * Raises: Not_found if [p] is not represented by an AI in [s].
 *)
val get_ai: prof -> state -> ai

(* [get_difficulty ai] is the difficulty level of the AI [ai].
 *)
val get_difficulty: ai -> difficulty

(* [still_in_game ai] is [true] iff the AI [ai] is still in the game.
 * If that ai is out, s/he can still prove suggestions wrong.
 *)
val still_in_game: ai -> bool

(************************************************
 * Methods for interacting with game state
 ************************************************)

(* [step ai state] peforms a turn for [ai]. This involves:
 *   - defining and setting goals by processing knowledge from suggestions and
 *     making deductions about other players' turns.
 *   - moving the AI around the map
 *   - forming and making suggestions/accusations
 * Returns: an updated game state.
 *)
val step:  ai -> state -> state

