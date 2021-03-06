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
  buildings:      building list;
  waiting_spots: (building * (coord list)) list;
  secrets:       (building * building) list;
  static_map:     string option array array;
  map_values:     string option array array;
  in_building:   (prof * building)list;
  location:      (prof * coord) list;
}

(* [user] stores information about the user. *)
type user = {
  character: prof;
  hand:      hand;
  was_moved: bool;
}

(* [ai] stores information about an ai. *)
type ai = {
  character:      prof;
  hand:           hand;
  difficulty:     difficulty;
  was_moved:      bool;
  is_in_game:     bool;
  possible_cards: card list;
  card_status:    (prof * ([`Y |`N |`Maybe |`Blank] array)) list;
}

(* [state] stores information about the entire game, including user's and ais'
 * information. *)
type state = {
  fact_file:     case_file;
  dictionary:    (prof * [`AI |`User |`No ]) list;
  game_complete: bool;
  counter:       int;
  map:           map;
  user:          user;
  ais:           ai list;
  past_guesses:  (case_file * prof * (prof option)) list;
}

(*[important_style] = style list for text under category: important *)
let important_style = 
  [ANSITerminal.on_black; ANSITerminal.Bold; ANSITerminal.green]

(*[info_style] = style list for text under category: game info *)
let info_style      = 
  [ANSITerminal.on_black; ANSITerminal.yellow]

(*[insn_style] = style list for text under category: instructions *)
let insn_style      = 
  [ANSITerminal.on_black; ANSITerminal.Bold; ANSITerminal.magenta]

(*[results_style] = style list for text under category: results *)
let results_style   = 
  [ANSITerminal.on_black; ANSITerminal.white]

(*[card_style] = style list for text that are cards*)
let card_style      = 
  [ANSITerminal.on_black; ANSITerminal.Bold; ANSITerminal.cyan]

(***** various functions *****)

(* [print_help] is a helper function for all other prints. Keeps the
 * output to 70 chars wide (width of preferred terminal width)
 * if it's shorter.
 *)
let print_help style s e =
  ANSITerminal.(
  if e then
  begin
    print_string [] (sprintf style "%-70s" s);
    print_endline ""
  end
  else print_string style s;)

(* [print_info s e] prints out text [s] under category: information.
 * if [e] = [true], then it will also return to new line, keeping output
 * at minimum width of 70 characters long. *)
let print_info s e = 
  print_help info_style s e

(* [print_insn s e] prints out text [s] under category: instructions.
 * if [e] = [true], then it will also return to new line, keeping output
 * at minimum width of 70 characters long. *)
let print_insn s e =
  print_help insn_style s e

(* [print_important s e] prints out text [s] under category: important.
 * if [e] = [true], then it will also return to new line, keeping output
 * at minimum width of 70 characters long. *)
let print_important s e = 
  print_help important_style s e

(* [print_results s e] prints out text [s] under category: results.
 * if [e] = [true], then it will also return to new line, keeping output
 * at minimum width of 70 characters long. *)
let print_results s e=
  print_help results_style s e

(* [print_card s e] prints out text [s] under category: card.
 * if [e] = [true], then it will also return to new line, keeping output
 * at minimum width of 70 characters long. *)
let print_card s e =
  print_help card_style s e

(* [print_info s e] prints out text [s].
 * if [e] = [true], then it will also return to new line, keeping output
 * at minimum width of 70 characters long. *)
let print s e =
  print_help [ANSITerminal.white; ANSITerminal.on_black] s e

(*[print_win] prints out the message when you win! *)
let print_win () =  
  print_important
  "   _____   _____   _____   _____   _  __   _____   _____   _____   _ " 
    true;
  print_important
  "  |  ___| |  _  | | ___ | |  ___| | |/__| |  _  | |__ __| |  ___| | |" 
    true;
  print_important 
  "  | |     | | | | | | | | | |  _  |  /    | |_| |   | |   | |___  |_|" 
    true;
  print_important 
  "  | |___  | |_| | | | | | | |_| | | |     | | | |   | |   |____ |  _ " 
    true;
  print_important 
  "  |_____| |_____| |_| |_| |_____| |_|     |_| |_|   |_|   |_____| |_|" 
    true;
  print_important " " true;
  print_info "CLUE will exit automatically. Feel free to play again!" true

(* [int_option_of_string s] is [Some i] if [s] can be converted to int [i]
 * using [int_of_string s], and [None] otherwise. *)
let int_option_of_string (s:string) : int option =
  try Some (int_of_string s)
  with Failure _ -> None

(* [diffculty_of_int] is the difficulty level corresponding to an int.
 * Requires: n is 1 or 2 or 3 *)
let difficulty_of_int (n:int) : difficulty =
  match n with
  | 1 -> Easy
  | 2 -> Medium
  | 3 -> Hard
  | _ -> failwith "This should not happen in difficulty_of_int"

let int_of_difficulty d =
  match d with
  | Easy -> 1
  | Medium -> 2
  | Hard -> 3

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
  | _ -> failwith ("Illegal int representation of building"^(string_of_int i))

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

(* [card_to_string c] is the string part of a card. *)
let card_to_string (c:card) : string =
  match c with
  | Prof s     -> s
  | Building s -> s
  | Language s -> s

(* [string_of_coord (r, c)] is the string representation of coord [(r, c)]. *)
let string_of_coord ((r,c):coord) : string =
  Printf.sprintf "(%d, %d)" r c

(* [string_of_exits exits] is the string representation of all exits to a
 * building with their ids and coordinates. 
 * Requires: [exits] has length 2 or 4. *)
let rec string_of_exits (exits:(int * coord) list) : string =
  match exits with
  | [e1;e2] -> (* Statler Hall *)
      ("  exit 1: (13, 8) upper right\n" ^
       "  exit 2: (16, 7) lower left")
  | [e1;e2;e3;e4] -> (* Klarman Hall *)
      ("  exit 1: (5,  9) upper left\n" ^
       "  exit 2: (8, 10) lower left\n" ^
       "  exit 3: (8, 15) lower right\n" ^
       "  exit 4: (5, 16) upper right")
  | _ -> failwith "This should not happen in string_of_exits in Data"

(* [print_exits] prints out the exits according to the game formatting *)
let print_exits exits =
  match exits with 
  | [e1; e2] ->
    print_results "  exit 1: (13, 8) upper right" true;
    print_results "  exit 2: (16, 7) lower left"  true;
  | [e1;e2;e3;e4] ->
    print_results "  exit 1: (5,  9) upper left"  true;
    print_results "  exit 2: (8, 10) lower left"  true;
    print_results "  exit 3: (8, 15) lower right" true;
    print_results "  exit 4: (5, 16) upper right" true;
  | _ -> failwith "This should not happen in string_of_exits in Data"


(* [string_of_prof_lst] is a comma-separated string representation of a list
 * of profs. *)
let rec string_of_prof_lst (lst:prof list) : string =
  match lst with
  | [] -> ""
  | [p] -> string_of_card (Prof p)
  | [p1;p2] -> (string_of_card (Prof p1)) ^ ", and " ^
               (string_of_card (Prof p2))
  | h::t -> (string_of_card (Prof h)) ^ ", " ^
            (string_of_prof_lst t)

(* [string_of_card_lst] is a comma-separated string representation of a list
 * of cards. *)
let rec string_of_card_lst (lst:card list) : string =
  match lst with
  | [] -> ""
  | [c] -> string_of_card c
  | [c1;c2] -> (string_of_card c1) ^ ", and " ^
               (string_of_card c2)
  | h::t -> (string_of_card h) ^ ", " ^
            (string_of_card_lst t)

(* [print_card_list] prints out a display of the cards according to format *)
let rec print_card_list lst =
  match lst with
  | []  -> ()
  | [c] -> print_card ("  "^(string_of_card c)) true
  | h::t -> 
    print_card ("  "^(string_of_card h)^", ") true;
    print_card_list t


(* [int_lst_to_prof_lst lst] is a prof list corresponding to int list [lst]. *)
let rec int_lst_to_prof_lst (lst:int list) : prof list =
  match lst with
  | [] -> []
  | h::t -> (prof_of_int h)::(int_lst_to_prof_lst t)

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

(* [card_lst_to_int_lst lst] is an int list corresponding to card list [lst].*)
let rec card_lst_to_int_lst (lst:card list) : int list =
  match lst with
  | [] -> []
  | h::t -> (int_of_card h)::(card_lst_to_int_lst t)

(* [card_lst_to_building_lst lst] is the building list corresponding to a
 * subset of the card list [lst]. *)
let card_lst_to_building_lst (lst:card list) : building list =
  lst |> card_lst_to_int_lst
      |> List.filter (fun i -> 6 <= i && i <= 14)
      |> List.map (fun x-> x-6)
      |> List.map (building_of_int)

(* [print_case_file cf] prints the case file [cf] in a sentence. *)
let print_case_file (cf:case_file) : unit =
  ANSITerminal.(
    let s = 
      (sprintf card_style "%s" ("Prof. "^cf.who))^
      (sprintf [white; on_black] "%s" " started the virus with ")^
      (sprintf card_style "%s" cf.with_what)^
      (sprintf [white; on_black] "%s" " in ")^
      (sprintf card_style "%-70s" (cf.where^" Hall"))^"\n"
    in
    print_string [] s;
    print_results "" true;
  )

(* [wait_for_user] waits for the user to hit enter to continue. *)
let wait_for_user () =
  print_info " " true;
  print_results
    "Press enter to continue..............................................."
    false;
  let _ = read_line () in 
    print_info " " true
