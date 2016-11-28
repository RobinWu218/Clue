open Data
open Gmap
open User 
open Ai

(* [init n d] is the initial game state with [n] AI bots and a difficulty level
 * of [d]. It prints out which character each player plays, and the cards in 
 * the user’s hands.
 * Requires: [n] is an integer between 2 and 5 inclusive, [d] is an integer
 * between 1 and 3 inclusive. *)
val init_state : int -> difficulty -> state

(* [step s] is the updated state after one player's turn. *)
val step : state -> state

(* [main n d] is the main entry point from outside this module to initialize 
 * a game with [n] AI bots and a difficulty level of [d] and start playing 
 * it. *)
val main : int -> int -> unit

