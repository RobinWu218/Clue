(* [InvalidLocation] is an exception raised by methods in the Map module *)
exception InvalidLocation

(* [Difficulty] are variants describing the different levels of AI play. *)
type difficulty = Easy | Medium | Expert

(* types for the game*)
(* professor who started the virus*)
type prof = string

(* building in where the virus started*)
type building = string

(* language that the perpetrator used*)
type language = string

(* card is the type that player can show to prove others' suggestions.
 * It can be a card describing a prof or a building or a language.*)
type card =
  | Prof of prof
  | Building of building
  | Language of language

(*The hand of what cards a person currently possesses.*)
type hand = card list

(* [card_of_int i] is the card representation of an integer from 0 to 20. *)
val card_of_int : int -> card

(* [int_of_card c] is the integer representation of a card. *)
val int_of_card : card -> int

(* case_file is the type defining who made the virus in which building with 
 * what kind of programming language.*)
type case_file = {who: prof; where: building; with_what: language}

(* integer pair representing a (row, column) on the map. *)
type coord = int * int

(* map is the type that holds information about the map. *)
type map = {
  num_rows: int;
  num_cols: int;
  map_values: string option array array;
  exits: building * (int * coord list);
  buildings: building list;
  in_buildling: (prof * building)list;
  location: prof * coord list;
  waiting_spots: build * coord list;
  secrets: (string*string) list;
}
(* user stores the information about the user's character, number of turns,
 * specific location and the language that s/he uses.*)
type user = {
  character: prof;
  hand: hand;
  was_moved: bool;
}

(* ai and player are almost the same except for that ai also has a list of
 * case_file that he obtains*)
type ai = {
  character : prof;
  hand: hand;
  was_moved: bool;
  is_in_game: bool;
  difficulty: int;
  destination: coord option;
  known_cards: card list;
  possible_cards: card list;
  past_guesses: (case_file * prof * (prof option)) list;
}

(* state is the type specifying the currect map situation and player's and 
 * ais' information. It also includes a case_file which was initiated at 
 * the init phase of the game. *)
type state = {
  counter: int;
  map: map;
  user: user;
  ais: ai list;
  fact_file: case_file;
  dictionary: prof * [ `AI | `User | `No ];
}
