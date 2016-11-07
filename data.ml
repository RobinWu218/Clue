(* [InvalidLocation] is an exception raised by methods in the Map module *)
exception InvalidLocation

(* [Difficulty] are variants describing the different levels of AI play. *)
type difficulty = Easy | Medium | Expert

(**************************************************************************************)


(* types for the game*)
(* professor who started the virus*)
type prof = Bracy | Clarkson | Fan | Gries | Halpern | White

(* building in where the virus started*)
type building = Baker | Carpenter | Duffield | Gates | Klarman |
				Olin Library | Phillips | Rhodes | Statler

(* language that the perpetrator used*)
type language = Bash | C | Java | MATLAB | OCaml | Python

(* card is the type that player can show to prove others' suggestions.
 * It can be a card describing a prof or a building or a language.*)
type card =
  | Prof of prof
  | Building of building
  | Language of language

(*
6 Professors:       
Anne Bracy 0, Michael Clarkson 1, Daisy Fan 2, David Gries 3, Joe Halpern 4, Walker White 5
9 Buildings:         
Baker Hall 6, Carpenter Hall 7, Duffield Hall 8, Gates Hall 9, Klarman Hall 10, Olin Library 11, Phillips Hall 12, Rhodes Hall 13, Statler Hall 14
6 Languages:       
Bash 15, C 16, Java 17, MATLAB 18, OCaml 19, Python 20
*)
(* [card_of_int i] is the card representation of an integer from 0 to 20 *)
let card_of_int  = failwith "TODO"

(* [int_of_card c] is the integer representation of a card *)
let int_of_card  = failwith "TODO"

(* case_file is the type defining who made the virus in which building with what kind
 * of programming language.*)
type case_file = {who: prof; where: building; with_what: language}

(* user stores the information about the user's character, number of turns,
 * specific location and the language that s/he uses.*)
type user = {
    character: prof;
    turn: int;
    location: int*int ; (*maybe specific location on the map?*)

}

(* ai and player are almost the same except for that ai also has a list of
 * case_file that he obtains*)
type ai = {
	character : prof;
	turn: int;
	location: int*int ; (*same as above*)
  difficulty: difficulty;
	known_cases: case_file list
}

(* state is the type specifying the currect map situation and player's and ais' information.
 * Also, it includes a fact_file which was initiated at the init phase of the game.*)
type state = {
    current_player: character;
	  map: map; (*need to be designed*)
    user: user;
    ais: ai list;
    fact_file: case_file

}


