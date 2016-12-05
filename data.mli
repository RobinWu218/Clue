(* [InvalidLocation] is raised by functions in the Map module. *)
exception InvalidLocation of string

(* [InvalidOperation] is raised by functions in the Map module. *)
exception InvalidOperation

(* [difficulty] represents the difficulty level of each AI player. *)
type difficulty = Easy | Medium | Hard

(* [prof] represents one of the six professors in game. *)
type prof = string

(* [building] represents one of the nine buildings in game. *)
type building = string

(* [language] represents one of the six languages in game. *)
type language = string

(* [card] represents a professor or a building or a language. *)
type card =
  | Prof of prof
  | Building of building
  | Language of language

(* [hand] represents the cards that a player has. *)
type hand = card list

(* [case_file] represents the fact file, a suggestion, or an accusation, which 
 * includes information of who started the virus in which building with which 
 * programming language. *)
type case_file = {who: prof; where: building; with_what: language}

(* [coord] represents a (row, column) coordinate on the map. *)
type coord = int * int

(* [dir] represents the direction and number of steps of a player's movement 
 * on the map. *)
type dir = Up of int | Down of int | Left of int | Right of int

(* [map] stores information about the map. *)
type map = {
  num_rows:       int;
  num_cols:       int;
  exits:         (building * ((int * coord) list)) list; 
       (* e.g., [("Gates",   [(1,    (0,0)); 
                             (2,    (5,5))     ])     ] *)
  buildings:      building list;
  waiting_spots: (building * (coord list)) list;
  secrets:       (building * building) list;
  (* Below are fields that can change throughout the game. *)
  map_values:     string option array array;
  in_building:   (prof * building)list;
  location:      (prof * coord) list;
}

(* [user] stores information about the user. *)
type user = {
  character: prof;
  hand:      hand;
  (* Below is a field that can change throughout the game. *)
  was_moved: bool;
}

(* [ai] stores information about an ai. *)
type ai = {
  character:      prof;
  hand:           hand;
  difficulty:     difficulty;
  (* Below are fields that can change throughout the game. *)
  was_moved:      bool;
  is_in_game:     bool;
  possible_cards: card list;
  card_status:    (prof * ([`Y |`N |`Maybe |`Blank] array)) list;
}

(* [state] stores information about the entire game, including user's and ais'
 * information. *)
type state = {
  fact_file:     case_file;
  dictionary:    (prof * [ `AI | `User | `No ]) list;
  (* Below are fields that can change throughout the game. *)
  game_complete: bool;
  counter:       int;
  map:           map;
  user:          user;
  ais:           ai list;
  past_guesses:  (case_file * prof * (prof option)) list;
}

val card_style : ANSITerminal.style list

(***** various functions *****)

val print_info : string -> bool -> unit

val print_insn : string -> bool -> unit

val print_important : string -> bool -> unit

val print_results : string -> bool -> unit

val print : string -> bool -> unit

(* [int_option_of_string s] is [Some i] if [s] can be converted to int [i]
 * using [int_of_string s], and [None] otherwise. *)
val int_option_of_string : string -> int option

(*TODO
 * Requires: n is 1 or 2 or 3 *)
val difficulty_of_int : int -> difficulty

(* [prof_of_int i] is the prof corresponding to an integer from 0 to 5. *)
val prof_of_int : int -> prof

(* [int_of_prof i] is the integer corresponding to a prof. *)
val int_of_prof : prof -> int

(* [building_of_int i] is the integer corresponding to a building. *)
val building_of_int : int -> building

(* [lang_of_int i] is the integer corresponding to a language. *)
val lang_of_int : int -> language

(* [card_of_int i] is the card representation of an integer from 0 to 20. *)
val card_of_int : int -> card

(* [int_of_card c] is the integer representation of a card. *)
val int_of_card : card -> int

(* [string_of_card c] is the string representation of a card. *)
val string_of_card : card -> string

(* [card_to_string c] is the string part of a card. *)
val card_to_string : card -> string

(* [string_of_exits exits] is the string representation of all exits to a 
 * building with their ids and coordinates. *)
val string_of_exits : (int * coord) list -> string

(* [string_of_prof_lst] is a comma-separated string representation of a list
 * of profs. *)
val string_of_prof_lst : prof list -> string

(* [string_of_card_lst] is a comma-separated string representation of a list
 * of cards. *)
val string_of_card_lst : card list -> string

(* [int_lst_to_prof_lst lst] is a prof list corresponding to int list [lst]. *)
val int_lst_to_prof_lst : int list -> prof list

(* [prof_lst_to_int_lst lst] is an int list corresponding to prof list 
 * [lst]. *)
val prof_lst_to_int_lst : prof list -> int list

(* [int_lst_to_card_lst lst] is a card list corresponding to int list [lst]. *)
val int_lst_to_card_lst : int list -> card list

(* [card_lst_to_int_lst lst] is an int list corresponding to card list [lst]. *)
val card_lst_to_int_lst : card list -> int list

(* [card_lst_to_building_lst lst] is the building list corresponding to a 
 * subset of the card list [lst]. *)
val card_lst_to_building_lst : card list -> building list

(* [print_case_file cf] prints the case file [cf] in a sentence. *)
val print_case_file : case_file -> unit

(* [wait_for_user] waits for the user to hit enter to continue. *)
val wait_for_user : unit -> unit
