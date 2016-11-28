(* [InvalidLocation] is raised by functions in the Map module. *)
exception InvalidLocation

(* [InvalidOperation] is raised by functions in the Map module. *)
exception InvalidOperation

(* [difficulty] represents the difficulty level of each AI player. *)
type difficulty = Easy | Medium | Expert

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

(* [prof_of_int i] is the prof corresponding to an integer from 0 to 5. *)
let prof_of_int (i:int) : prof =
  match i with
  | 0 -> "Bracy"
  | 1 -> "Clarkson"
  | 2 -> "Fan"
  | 3 -> "Gries"
  | 4 -> "Halpern"
  | 5 -> "White"
  | _ -> failwith "Illegal int representation of a prof"

(* [int_of_prof i] is the integer corresponding to a prof. *)
let int_of_prof (p:prof) : int =
  match p with 
  | "Bracy"     -> 0
  | "Clarkson"  -> 1
  | "Fan"       -> 2
  | "Gries"     -> 3
  | "Halpern"   -> 4
  | "White"     -> 5
  | _           -> failwith "Illegal string representation of a prof"

(* [building_of_int i] is the integer corresponding to a building. *)
let building_of_int (i:int) : building =
  match i with
  | 0 -> "Baker"
  | 1 -> "Carpenter"
  | 2 -> "Duffield"
  | 3 -> "Gates"
  | 4 -> "Klarman"
  | 5 -> "Olin"
  | 6 -> "Phillips"
  | 7 -> "Rhodes"
  | 8 -> "Statler"
  | _ -> failwith "Illegal int representation of a building"

(* [lang_of_int i] is the integer corresponding to a language. *)
let lang_of_int (i:int) : language =
  match i with
  | 0 -> "Bash"
  | 1 -> "C"
  | 2 -> "Java"
  | 3 -> "MATLAB"
  | 4 -> "OCaml"
  | 5 -> "Python"
  | _ -> failwith "Illegal int representation of a language"

(* [card_of_int i] is the card representation of an integer from 0 to 20. *)
let card_of_int (i:int) : card =
  match i with
  | 0  -> Prof "Bracy"
  | 1  -> Prof "Clarkson"
  | 2  -> Prof "Fan"
  | 3  -> Prof "Gries"
  | 4  -> Prof "Halpern"
  | 5  -> Prof "White"
  | 6  -> Building "Baker"
  | 7  -> Building "Carpenter"
  | 8  -> Building "Duffield"
  | 9  -> Building "Gates"
  | 10 -> Building "Klarman"
  | 11 -> Building "Olin"
  | 12 -> Building "Phillips"
  | 13 -> Building "Rhodes"
  | 14 -> Building "Statler"
  | 15 -> Language "Bash"
  | 16 -> Language "C"
  | 17 -> Language "Java"
  | 18 -> Language "MATLAB"
  | 19 -> Language "OCaml"
  | 20 -> Language "Python"
  | _  -> failwith "Illegal int representation of a card"

(* [int_of_card c] is the integer representation of a card. *)
let int_of_card (c:card) : int =
  match c with
  | Prof "Bracy"          -> 0
  | Prof "Clarkson"       -> 1
  | Prof "Fan"            -> 2
  | Prof "Gries"          -> 3
  | Prof "Halpern"        -> 4
  | Prof "White"          -> 5
  | Building "Baker"      -> 6
  | Building "Carpenter"  -> 7
  | Building "Duffield"   -> 8
  | Building "Gates"      -> 9
  | Building "Klarman"    -> 10
  | Building "Olin"       -> 11
  | Building "Phillips"   -> 12
  | Building "Rhodes"     -> 13
  | Building "Statler"    -> 14
  | Language "Bash"       -> 15
  | Language "C"          -> 16
  | Language "Java"       -> 17
  | Language "MATLAB"     -> 18
  | Language "OCaml"      -> 19
  | Language "Python"     -> 20
  | _ -> failwith "Illegal card"

(* [string_of_card c] is the string representation of a card. *)
let string_of_card (c:card) : string =
  match c with
  | Prof s     -> "Prof. " ^ s
  | Building s -> s ^ " Hall"
  | Language s -> s

(* [lst_to_prof_lst lst] is a prof list corresponding to int list [lst]. *)
let rec lst_to_prof_lst (lst:string list) : prof list =
  match lst with
  | [] -> []
  | h::t -> (prof_of_int h)::(lst_to_prof_lst t)

(* [prof_lst_to_int_lst lst] is an int list corresponding to prof list 
 * [lst]. *)
let rec prof_lst_to_int_lst (lst:prof list) : int list =
  match lst with
  | [] -> []
  | h::t -> (int_of_prof h)::(prof_lst_to_int_lst t)

(* [int_lst_to_card_lst lst] is a card list corresponding to int list [lst]. *)
let rec int_lst_to_card_lst (lst:int list) : card list =
  match lst with
  | [] -> []
  | h::t -> (card_of_int h)::(int_lst_to_card_lst t)

(* [card_lst_to_int_lst lst] is an int list corresponding to card list [lst]. *)
let rec card_lst_to_int_lst (lst:card list) : int list =
  match lst with
  | [] -> []
  | h::t -> (int_of_card h)::(card_lst_to_int_lst t)

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
  character :     prof;
  hand:           hand;
  difficulty:     difficulty;
  (* Below are fields that can change throughout the game. *)
  was_moved:      bool;
  is_in_game:     bool;
  destination:    coord option;
  known_cards:    card list;
  possible_cards: card list;
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
