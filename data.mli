
(* [InvalidLocation] is an exception raised by methods in the Map module *)
exception InvalidLocation

(* [difficulty] are variants describing the different levels of AI play. *)
type difficulty

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

(*The hand of what cards a person currently possesses.*)
type hand

(*
6 Professors:
Anne Bracy 0, Michael Clarkson 1, Daisy Fan 2, David Gries 3, Joe Halpern 4, Walker White 5
9 Buildings:
Baker Hall 6, Carpenter Hall 7, Duffield Hall 8, Gates Hall 9, Klarman Hall 10, Olin Library 11, Phillips Hall 12, Rhodes Hall 13, Statler Hall 14
6 Languages:
Bash 15, C 16, Java 17, MATLAB 18, OCaml 19, Python 20
*)

(* [card_of_int i] is the card representation of an integer from 0 to 20 *)
val card_of_int : int -> card

(* [int_of_card c] is the integer representation of a card *)
val int_of_card : card -> int

(* case_file is the type defining who made the virus in which building with what kind
 * of programming language.*)
type case_file

(* integer pair representing a (row, column) on the map. *)
type coord

(* map is the data structure that holds all data about the map and people locations *)
type map

(* player stores the information about the player's character, number of turns,
 * specific location and the language that he uses.*)
type user

(* ai and player are almost the same except for that ai also has a list of
 * case_file that he obtains*)
type ai

(* state is the type specifying the currect map situation and player's and ais' information.
 * Also, it includes a fact_file which was initiated at the init phase of the game.*)
type state