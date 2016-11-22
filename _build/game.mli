open Data
open Gmap
open Ai
open User

(******************************)
(* init_state and its helpers *)
(******************************)

(* [generate_case_file ()] is the case file containing the answers to the 
 * questions: Who? Where? What language? *)
val generate_case_file : unit -> case_file

(* [assign_characters n] is an n-tuple of non-repeating profs.
 * Requires: [n] is an integer between 3 and 6 inclusive. *)
val assign_characters : int -> prof list (* or tuple? *)

(* [deal_card n] is an n-tuple of non-repeating card lists. 
 * Requires: [n] is an integer between 3 and 6 inclusive. *)
val deal_card : int -> card list

(* [init n d] is the initial game state with [n] AI bots and a difficulty 
 * level of [d]. It prints out which character each player plays, and the 
 * cards in the user’s hands.
 * Requires: [n] is an integer between 2 and 5 inclusive, [d] is an integer 
 * between 1 and 3 inclusive. *)
val init_state : int -> int -> state

(************************)
(* repl and its helpers *)
(************************)

(* [roll_two_dice ()] is the sum of two random integers between 1 and 6 
 * inclusive. It also prints the two integers and the sum. *)
val roll_two_dice : unit -> int

(* [update_state c s] is the new state after command [c] is executed when  
 * the current state is [s]. *)
val update_state : string -> state -> state

(* [repl turn s] either calls itself for a new turn or returns [()] when the
 * game terminates.
 * Requires: [turn] is a nonnegative integer. *)
val repl : int -> state -> unit

(********)
(* main *)
(********)

(* [main n d] is the main entry point from outside this module to initialize 
 * a game with [n] AI bots and a difficulty level of [d] and start playing 
 * it. 
 * Requires: [n] is an integer between 2 and 5 inclusive, [d] is an integer 
 * between 1 and 3 inclusive. *)
val main : int -> int -> unit