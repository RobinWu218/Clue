open Map

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

(* case_file is the type defining who made the virus in which building with what kind
 * of programming language.*)
type case_file = {who: prof; where: building; with_what: language}

(* player stores the information about the player's character, number of turns,
 * specific location and the language that he uses.*)
type player = {

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
	known_cases: case_file list 
}

(* state is the type specifying the currect map situation and player's and ais' information.
 * Also, it includes a fact_file which was initiated at the init phase of the game.*)
type state = {
    current_player: character;
	map: map; (*need to be designed*)
    player: player;
    ais: character list;
    fact_file: case_file
    
}


