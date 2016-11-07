
(* [InvalidLocation] is an exception raised by methods in the Map module *)
exception InvalidLocation

(* [Difficulty] are variants describing the different levels of AI play. *)
type Difficulty 

(* types for the game*)
(* professor who started the virus*)
type prof 

(* building in where the virus started*)
type building 

(* language that the perpetrator used*)
type language 

(* card is the type that player can show to prove others' suggestions.
 * It can be a card describing a prof or a building or a language.*)
type card 

(* case_file is the type defining who made the virus in which building with what kind
 * of programming language.*)
type case_file 

(* player stores the information about the player's character, number of turns,
 * specific location and the language that he uses.*)
type player 

(* ai and player are almost the same except for that ai also has a list of
 * case_file that he obtains*)
type ai 

(* state is the type specifying the currect map situation and player's and ais' information.
 * Also, it includes a fact_file which was initiated at the init phase of the game.*)
type state 