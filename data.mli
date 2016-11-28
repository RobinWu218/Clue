(* [InvalidLocation] is an exception raised by methods in the Map module *)
exception InvalidLocation
exception InvalidOperation

(* [Difficulty] are variants describing the different levels of AI play. *)
type difficulty = Easy | Medium | Expert

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

(* [string_of_card c] is the string representation of a card. *)
val string_of_card : card -> string

(* case_file is the type defining who made the virus in which building with 
 * what kind of programming language.*)
type case_file = {who: prof; where: building; with_what: language}

(* integer pair representing a (row, column) on the map. *)
type coord = int * int

(* map is the type that holds information about the map. *)
type map = {
  num_rows:      int;
  num_cols:      int;
  map_values:    string option array array;
  exits:        (building * ((int * coord) list)) list; 
             (* ("Gates",   [ (1, (0,0)); (2,(5,5)) ] *)
  buildings:     building list;
  in_building:  (prof * building)list;
  location:     (prof * coord) list;
  waiting_spots:(building * (coord list)) list;
  secrets:      (building * building) list;
}

(* user stores the information about the user's character, number of turns,
 * specific location and the language that s/he uses.*)
type user = {
  character: prof;
  hand:      hand;
  was_moved: bool;
}

(* ai and player are almost the same except for that ai also has a list of
 * case_file that he obtains*)
type ai = {
  character :   prof;
  hand:         hand;
  was_moved:    bool;
  is_in_game:   bool;
  difficulty:   difficulty;
  destination:  coord option;
  known_cards:  card list;
  possible_cards: card list;
  past_guesses:  (case_file * prof * (prof option)) list;
}

(* state is the type specifying the currect map situation and player's and ais' information.
 * Also, it includes a fact_file which was initiated at the init phase of the game.*)
type state = {
  counter:  int;
  game_complete: bool;
  map:      map;
  user:     user;
  ais:      ai list;
  fact_file:   case_file;
  dictionary: (prof * [ `AI | `User | `No ]) list;
}

(* [assign_was_moved s p b] assigns bool [b] to the [was_moved] field of
 * whoever playing the character of prof [p] in state [s]. If no one plays that
 * character, then [s] is simply unchanged. *)
val assign_was_moved : state -> prof -> bool -> state

(* [roll_two_dice ()] simulates rolling two dice, prints the results, and 
 * returns the sum. *)
val roll_two_dice : unit -> int

