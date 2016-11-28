(* [InvalidLocation] is an exception raised by methods in the Map module *)
exception InvalidLocation
exception InvalidOperation

(* [Difficulty] are variants describing the different levels of AI play. *)
type difficulty = Easy | Medium | Expert

(*****************************************************************************)

(* types for the game *)
(* professor who started the virus *)
type prof = string

(* building in where the virus started *)
type building = string

(* language that the perpetrator used *)
type language = string

(* card is the type that player can show to prove others' suggestions.
 * It can be a card describing a prof or a building or a language. *)
type card =
  | Prof of prof
  | Building of building
  | Language of language

(*The hand of what cards a person currently possesses. *)
type hand = card list

(* [card_of_int i] is the card representation of an integer from 0 to 20. *)
let card_of_int (i:int) : card =
  match i with
  | 0 -> Prof "Bracy"
  | 1 -> Prof "Clarkson"
  | 2 -> Prof "Fan"
  | 3 -> Prof "Gries"
  | 4 -> Prof "Halpern"
  | 5 -> Prof "White"
  | 6 -> Building "Baker"
  | 7 -> Building "Carpenter"
  | 8 -> Building "Duffield"
  | 9 -> Building "Gates"
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
  | _ -> failwith "Illegal int representation of card"

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
  | Prof s -> "Prof. " ^ s
  | Building s -> s ^ " Hall"
  | Language s -> s

(* case_file is the type defining who made the virus in which building with
 * which programming language. *)
type case_file = {who: prof; where: building; with_what: language}

(* integer pair representing a (row, column) on the map. *)
type coord = int * int

(* dir is the type representing player movement *)
type dir = Up of int | Down of int | Left of int | Right of int

(* map is the type that holds information about the map. *)
type map = {
  num_rows: int;
  num_cols: int;
  map_values: string option array array;
  exits: (building * ((int * coord) list)) list; (* ("Gates", [ (1, (0,0)); (2,(5,5))] *)
  buildings: building list;
  in_building: (prof * building)list;
  location: (prof * coord) list;
  waiting_spots: (building * (coord list)) list;
  secrets: (building * building) list;
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
  difficulty: difficulty;
  destination: coord option;
  known_cards: card list;
  possible_cards: card list;
  past_guesses: (case_file * prof * (prof option)) list;
}

(* state is the type specifying the currect map situation and player's and ais' information.
 * Also, it includes a fact_file which was initiated at the init phase of the game.*)
type state = {
  counter: int;
  game_complete: bool;
  map: map;
  user: user;
  ais: ai list;
  fact_file: case_file;
  dictionary: prof * [ `AI | `User | `No ] list;
}

(* [assign_was_moved s p b] assigns bool [b] to the [was_moved] field of
 * whoever playing the character of prof [p] in state [s]. If no one plays that
 * character, then [s] is simply unchanged. *)
let assign_was_moved (s:state) (p:prof) (b:bool) : state =
  match List.assoc p dictionary with
  | `AI ->
      let newais = List.map
                   (fun a -> if a.character = p
                             then {a with was_moved = b}
                             else a) s.ais in
      {s with ais = newais}
  | `User ->
      let newuser = {s.user with was_moved = b} in
      {s with user = newuser}
  | `No ->
      s


(* [roll_two_dice ()] simulates rolling two dice, prints the results, and
 * returns the sum. *)
let roll_two_dice () : int =
  let d1 = 1 + Random.int 5 in
  let d2 = 1 + Random.int 5 in
  let sum = d1 + d2 in
  print_endline "Rolling two dice...";
  Printf.printf "Die 1: %d\n" d1;
  Printf.printf "Die 2: %d\n" d2;
  sum
