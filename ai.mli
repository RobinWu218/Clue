open Data
open Gmap

(* [init p h d lst] is the AI data structure that represents an AI playing
 * character [p] on difficulty level [d], with hand [h]. [lst] is a list of 
 * all players initialized in the game.
 *)
val init: prof -> hand -> difficulty -> prof list -> ai

(* [step ai state] peforms a turn for [ai]. This involves:
 *   - defining and setting goals by processing knowledge from suggestions and
 *     making deductions about other players' turns.
 *   - moving the AI around the map
 *   - forming and making suggestions/accusations
 * Returns: an updated game state.
 *)
val step: ai -> state -> state

