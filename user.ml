open Data
open Gmap

(* [get_who ()] prompts the user for the professor s/he wants to suggest
 * or accuse and returns the corresponding string. *)
let rec get_who () : string =
  print_endline "Who did it? [Bracy/Clarkson/Fan/Gries/Halpern/White]";
  let str = print_string  "> "; read_line () in
  let str' = String.(str |> trim |> lowercase_ascii) in
  if String.length str' = 0 then
    print_endline "Please at least type something!"; get_who ()
  else 
    match str'.[0] with 
    | 'b' -> "Bracy"
    | 'c' -> "Clarkson"
    | 'f' -> "Fan"
    | 'g' -> "Gries"
    | 'h' -> "Halpern"
    | 'w' -> "White"
    | _ -> print_endline "Invalid input; try again please."; get_who ()

(* [get_where ()] prompts the user for the builidng s/he wants to accuse
 * and returns the corresponding string. *)
let rec get_where () : string =
  print_endline ("Where? [Baker/Carpenter/Duffield/Gates/Klarman/Olin/" ^
                          "Phillips/Rhodes/Statler]");
  let str = print_string  "> "; read_line () in
  let str' = String.(str |> trim |> lowercase_ascii) in
  if String.length str' = 0 then
    print_endline "Please at least type something!"; get_where ()
  else 
    match str'.[0] with 
    | 'b' -> "Baker"
    | 'c' -> "Carpenter"
    | 'd' -> "Duffield"
    | 'g' -> "Gates"
    | 'k' -> "Klarman"
    | 'o' -> "Olin"
    | 'p' -> "Phillips"
    | 'r' -> "Rhodes"
    | 's' -> "Statler"
    | _ -> print_endline "Invalid input; try again please."; get_where ()

(* [get_with_what ()] prompts the user for the language s/he wants to suggest
 * or accuse and returns the corresponding string. *)
let rec get_with_what () : string =
  print_endline "With what language? [Bash/C/Java/MATLAB/OCaml/Python]";
  let str = print_string  "> "; read_line () in
  let str' = String.(str |> trim |> lowercase_ascii) in
  if String.length str' = 0 then
    print_endline "Please at least type something!"; get_with_what ()
  else 
    match str'.[0] with 
    | 'b' -> "Bash"
    | 'c' -> "C"
    | 'j' -> "Java"
    | 'm' -> "MATLAB"
    | 'o' -> "OCaml"
    | 'p' -> "Python"
    | _ -> print_endline "Invalid input; try again please."; get_with_what ()

(* [user_accuse s] prompts the user for his/her accusation, determines whether the
 * user wins or not, and ends the game by returning the updated state. *)
let user_accuse (s:state) : state =
  let who = get_who () in
  let where = get_where () in
  let with_what = get_with_what () in
  let map = teleport_professor s.map (Prof who) (Building where) in
  let accusation = 
    {who = Prof who; 
     where = Building where; 
     with_what = Language with_what} in
  if accusation = s.fact_file then
    print_endline "Awesome! You got the right accusation.";
    print_endline "YOU WIN!!!";
    print_endline "Clue will exit automatically. Do come again!";
    {s with s.game_complete = true; s.map = map}
  else
    print_endline "Uh-oh, wrong accusation."
    print_endline "Unfortunately, you have just lost the game. :("
    print_endline "The real case file is: \n"
    Printf.printf "Prof. %s created the virus with %s in %s Hall.\n" 
                  who with_what where;
    print_endline "Clue will exit automatically. Do come again!";
    {s with s.game_complete = true; s.map = map}

(* [accuse_or_not s] asks the user whether s/he wants to make an accusation or
 * not, and if so, updates the current state [s] by calling [accuse s]. *)
let rec accuse_or_not (s:state) : state =
  print_endline "Do you want to accuse? [y/n]";
  let str = print_string  "> "; read_line () in
  match String.(str |> trim |> lowercase_ascii) with 
  | "y" -> user_accuse s
  | "n" -> s
  | _ -> print_endline "Invalid command; try again please."; accuse_or_not s

(* [user_suggest s] prompts the user for his/her suggestion and calls 
 * ai_disprove until it is disproved or all passed TODO. *)
let user_suggest (s:state) : state =
  print_endline "Please make a suggestion about the current building now.";
  let who = get_who () in
  let where_option = get_current_building s1.map s1.user.character in
  let with_what = get_with_what () in
  match where_option with
  | Some where ->
      let map = teleport_professor s.map (Prof who) (Building where) in
      let suggestion = 
        {who = Prof who; 
         where = Building where; 
         with_what = Language with_what} in
      ai_disprove ai suggestion
      (* TODO
B->C->F->G->H->W start from the one after user’s character and see 
if ai can disprove the suggestion
If disproved, Accuse or not? If not -> end turn
If not disproved, user choose to make accusation or end turn
update map
AI past_guesses update?
      *)
  | None -> failwith "This should not happen in user_suggest"

(* [int_option_of_string s] is [Some i] if [s] can be converted to int [i]
 * using [int_of_string s], and [None] otherwise. *)
let int_option_of_string (s:string) : int option =
  try Some (int_of_string s)
  with Failure _ -> None

(* [get_movement n] prompts the user for the next steps s/he wants to take
 * and, if feasible, returns the corresponding tuple representation of the
 * desired movement. *)
let rec get_movement (n:int) : string * int =
  Printf.printf "You can move %d steps... [up/down/left/right i]\n" n;
  let str = print_string  "> "; read_line () |> String.lowercase_ascii in
  let lst = Str.(str |> split (regexp "[ ]+")) in
  match lst with
  | dir::xstr::[] ->
      let x = int_option_of_string xstr in
      begin
        match dir.[0], x with
        | _,   Some x' when ((x' > n) || (x' <= 0)) ->         
            print_endline "Invalid number of steps; try again please."; 
            get_movement n
        | 'u', Some x' -> ("up",    x')
        | 'd', Some x' -> ("down",  x')
        | 'l', Some x' -> ("left",  x')
        | 'r', Some x' -> ("right", x')
        | _ -> print_endline "Invalid input; try again please."; 
               get_movement n
      end
  | _ -> print_endline "Invalid input; try again please."; get_movement n

(* [user_move n s] prompts the user to enter commands so that his/her 
 * character moves n steps, or fewer if the character gets into a building
 * before using up all the steps. *)
let rec user_move (n:int) (s:state) : state =
  if n < 0 then 
    failwith "This should not happen in user_move"
  if n = 0 then 
    print_endline "You cannot move anymore.";
    if is_in_building s.map s.user.character then user_suggest s
    else s
  else
    let (dir, x) = get_movement n in
    let (y, map) = move s.map s.user.character dir x in
    user_move (n-x+y) {s with s.map = map}

(* [use_secret b s] is the updated state after the user enters building [b]
 * via a secret passage. *)
let use_secret (b:building) (s:state) : state =
  let map = move_to_building s.map s.user.character b in (*TODO done in gmap.ml?*)
  user_suggest {s with s.map = map}

(* [get_choice ()] is [true] if the user selects the first choice and [false]
 * if the user selects the second choice. *)
let rec get_choice () : bool =
  let str = print_string  "> "; read_line () in
  let str' = String.(str |> trim |> lowercase_ascii) in
  if String.length str' = 0 then
    print_endline "Please at least type something!"; get_choice ()
  else 
    match str'.[0] with 
    | '1' -> true
    | '2' -> false

(* [suggest_or_secret b s] prompts the user to choose between making a suggestion
 * and using the secret passage to enter building [b], and returns the updated 
 * state. *)
let suggest_or_secret (b:building) (s:state) : state =
  Printf.printf ("You can either 1 make a suggestion now or 2 use the " ^
                 "secret passage to get into %s Hall. [1/2]\n") b;
  match get_choice () with 
  | true -> user_suggest s
  | false -> use_secret b s

(* [secret_or_roll b s] prompts the user to choose between using the secret passage
 * to enter building [b] and rolling the dice to move out, and returns the updated 
 * state. *)
let secret_or_roll (b:building) (s:state) : state =
  Printf.printf ("You can either 1 use the secret passage to get into %s " ^
                 "Hall or 2 roll the dice and move out. [1/2]\n") b;
  match get_choice () with 
  | true -> use_secret b s
  | false -> user_move (roll_two_dice ()) s

(* [secret_or_roll b s] prompts the user to choose between making a suggestion
 * and rolling the dice to move out, and returns the updated state. *)
let suggest_or_roll (s:state) : state =
  Printf.printf ("You can either 1 make a suggestion now or 2 roll the " ^
                 "dice and move out. [1/2]\n") b;
  match get_choice () with 
  | true -> user_suggest s
  | false -> user_move (roll_two_dice ()) s

(* [in_building_involuntarily b s] checks whether there is a secrect passage
 * in user's current building [b] and whether exits to [b] are all blocked, 
 * determines what the user can do given that the user was moved to the 
 * building by someone else, and returns the updated state accordingly. *)
let in_building_involuntarily (b:building) (s:state) : state =
  let secret = get_secret s.map b in (*TODO done in gmap.ml?*)
  let blocked = is_building_blocked s.map b in
  match secret, blocked with
  | Some b', true  -> 
      print_endline "There is a secret passage available.";
      print_endline "All exits to the current building are blocked."; 
      suggest_or_secret b' s
  | Some b', false -> 
      print_endline "There is a secret passage available.";
      secret_or_roll b' s
  | None,    true  -> 
      print_endline "All exits to the current building are blocked."; 
      user_suggest s
  | None,    false -> 
      suggest_or_roll s

(* [in_building_voluntarily b s] checks whether there is a secrect passage
 * in user's current building [b] and whether exits to [b] are all blocked, 
 * determines what the user can do given that the user entered the building
 * on his/her own, and returns the updated state accordingly. *)
let in_building_voluntarily (s:state) : state =
  let secret = get_secret s.map b in (*TODO done in gmap.ml?*)
  let blocked = is_building_blocked s.map b in
  match secret, blocked with
  | Some b', true  -> 
      print_endline "There is a secret passage available.";
      print_endline "All exits to the current building are blocked."; 
      print_endline "You have to use the secret passage.";
      use_secret b' s
  | Some b', false -> 
      print_endline "There is a secret passage available.";
      secret_or_roll b' s
  | None,    true  -> 
      print_endline "There is no secret passage available.";
      print_endline "All exits to the current building are blocked."; 
      print_endline "You have to wait until your next turn.";
      s
  | None,    false -> 
      user_move (roll_two_dice ()) s

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

(* [user_turn s] is the new state after the user finishes his/her turn when
 * the current state is [s]. *)
let user_turn (s:state) : state =
  let s1 = accuse_or_not s in
  if s1.game_complete then s1 else 
  match get_current_building s1.map s1.user.character with
  | Some b ->
      if s1.user.was_moved 
      then in_building_involuntarily b s1
      else in_building_voluntarily b s1
  | None ->
      user_move (roll_two_dice ()) s1
