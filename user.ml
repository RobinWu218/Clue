open Data
open Gmap

(********************)
(* helper functions *)
(********************)

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

(* [user_accuse s] prompts the user for his/her accusation, determines whether 
 * the user wins or not, and ends the game by returning the updated state.  
 * Calls [teleport_professor] to move the suggested prof's corresponding ai 
 * player to the suggested building and change that ai's was_moved field to 
 * true if ai is not already in the building before being moved. *)
let user_accuse (s:state) : state =
  let who = get_who () in
  let where = get_where () in
  let with_what = get_with_what () in
  let (moved_or_not, map) = 
    if get_current_building s.map who <> (Some where)
    then (true, (teleport_professor s.map who where))
    else (false, s.map) in
  let accusation = 
    {who = Prof who; 
     where = Building where; 
     with_what = Language with_what} in
  if accusation = s.fact_file then
    print_endline "Awesome! You got the right accusation.";
    print_endline "YOU WIN!!!";
    print_endline "Clue will exit automatically. Do come again!";
    let news = {s with game_complete = true; map = map} in
    assign_was_moved news who moved_or_not
  else
    print_endline "Uh-oh, wrong accusation."
    print_endline "Unfortunately, you have just lost the game. :("
    print_endline "The real case file is: \n"
    Printf.printf "Prof. %s created the virus with %s in %s Hall.\n" 
                  who with_what where;
    print_endline "Clue will exit automatically. Do come again!";
    let news = {s with game_complete = true; map = map} in
    assign_was_moved news who moved_or_not

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
 * ai_disprove until it is disproved or all passed. Calls [teleport_professor]
 * to move the suggested prof's corresponding ai player to the suggested 
 * building and change that ai's was_moved field to true. *)
let user_suggest (s:state) : state =
  print_endline "Please make a suggestion about the current building now.";
  let who = get_who () in
  let where_option = get_current_building s1.map s1.user.character in
  let with_what = get_with_what () in
  match where_option with
  | Some where ->
      let (moved_or_not, map) = 
        if get_current_building s.map who <> (Some where)
        then (true, (teleport_professor s.map who where))
        else (false, s.map) in
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
      let news = {s with s.game_complete = true; s.map = map} in
      assign_was_moved news who moved_or_not
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
    user_move (n-x+y) {s with map = map}

(* [use_secret s] is the updated state after the user uses the secret passage
 * in the current building. *)
let use_secret (s:state) : state =
  let map = use_secret_passage s.map s.user.character in
  user_suggest {s with map = map}

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
    | _ -> print_endline "Please type either 1 or 2!"; get_choice ()

(* [get_choice_three ()] is [1] if the user selects the first choice, [2] if
 * the user selects the second choice, and [3] if the third. *)
let rec get_choice_three () : int =
  let str = print_string  "> "; read_line () in
  let str' = String.(str |> trim |> lowercase_ascii) in
  if String.length str' = 0 then
    print_endline "Please at least type something!"; get_choice ()
  else 
    match str'.[0] with 
    | '1' -> 1
    | '2' -> 2
    | '1' -> 3
    | _ -> print_endline "Please type 1 or 2 or 3!"; get_choice ()

(* [suggest_or_secret b s] prompts the user to choose between making a 
 * suggestion and using the secret passage to enter building [b], and returns 
 * the updated state. *)
let suggest_or_secret (b:building) (s:state) : state =
  Printf.printf ("You can either 1 make a suggestion now or 2 use the " ^
                 "secret passage to get into %s Hall. [1/2]\n") b;
  match get_choice () with 
  | true -> user_suggest s
  | false -> use_secret b s

(* [secret_or_roll b s] prompts the user to choose between using the secret 
 * passage to enter building [b] and rolling the dice to move out, and returns 
 * the updated state. *)
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
  let secret = has_secret_passage s.map b in
  let blocked = is_building_blocked s.map b in
  match secret, blocked with
  | true,  true  -> 
      print_endline "There is a secret passage available.";
      print_endline "All exits to the current building are blocked."; 
      suggest_or_secret s
  | true,  false -> 
      print_endline "There is a secret passage available.";
      secret_or_roll s
  | false, true  -> 
      print_endline "All exits to the current building are blocked."; 
      user_suggest s
  | false, false -> 
      suggest_or_roll s

(* [in_building_voluntarily b s] checks whether there is a secrect passage
 * in user's current building [b] and whether exits to [b] are all blocked, 
 * determines what the user can do given that the user entered the building
 * on his/her own, and returns the updated state accordingly. *)
let in_building_voluntarily (b:building) (s:state) : state =
  let secret = has_secret_passage s.map b in
  let blocked = is_building_blocked s.map b in
  match secret, blocked with
  | true,  true  -> 
      print_endline "There is a secret passage available.";
      print_endline "All exits to the current building are blocked."; 
      print_endline "You have to use the secret passage.";
      use_secret s
  | true,  false -> 
      print_endline "There is a secret passage available.";
      secret_or_roll s
  | false, true  -> 
      print_endline "There is no secret passage available.";
      print_endline "All exits to the current building are blocked."; 
      print_endline "You have to wait until your next turn.";
      s
  | false, false -> 
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

(* [choose_from_two c1 c2] is [Some c1] or [Some c2] as determined by user. *)
let choose_from_two (c1:card) (c2:card) : card option =
  Printf.printf "You can reveal either card 1: %s, or card 2: %s. [1/2]\n"
                (string_of_card c1) (string_of_card c2);
  match get_choice () with 
  | true -> Some c1
  | false -> Some c2

(* [choose_from_three c1 c2 c3] is [Some c1] or [Some c2] or [Some c3] as 
 * determined by user. *)
let choose_from_three (c1:card) (c2:card) (c3:card) : card option =
  Printf.printf ("You can reveal one of the three cards: \n" ^
                 "card 1: %s, card 2: %s, or card 3: %s. [1/2/3]\n")
                (string_of_card c1) (string_of_card c2) (string_of_card c3);
  match get_choice_three () with 
  | 1 -> Some c1
  | 2 -> Some c2
  | 3 -> Some c3
  | _ -> failwith "This should not happen in choose_from_three in user.ml"

(******************)
(* main functions *)
(******************)

(* [user_step s] is the new state after the user finishes his/her turn when
 * the current state is [s]. *)
let user_step (s:state) : state =
  let s1 = accuse_or_not s in
  if s1.game_complete then s1 else 
  match get_current_building s1.map s1.user.character with
  | Some b ->
      if s1.user.was_moved 
      then in_building_involuntarily b s1
      else in_building_voluntarily b s1
  | None ->
      user_move (roll_two_dice ()) s1

(* [user_disprove s guess] is [None] if the user does not have any card
 * to disprove the suggestion [guess] and a card option if the user has the 
 * card(s) and wishes to disprove [guess] with that card. *)
let user_disprove (s:state) (guess:case_file) : card option =
  print_endline "It is your turn to disprove the suggstion:";
  let hand = s.user.hand in
  let {who; where; with_what} = guess in
  let who_or_not = List.mem (Prof who) hand in
  let where_or_not = List.mem (Building where) hand in
  let with_what_or_not = List.mem (Language with_what) hand in
  match who_or_not, where_or_not, with_what_or_not with
  | true, true, true ->
      print_endline ("You have three cards to disprove the suggestion" ^
                     "and you have to reveal one of them.");
      choose_from_three (Prof who) (Building where) (Language with_what)
  | true, true, false ->
      print_endline ("You have two cards to disprove the suggestion" ^
                     "and you have to reveal one of them.");
      choose_from_two (Prof who) (Building where)
  | true, false, true ->
      print_endline ("You have two cards to disprove the suggestion" ^
                     "and you have to reveal one of them.");
      choose_from_two (Prof who) (Language with_what)
  | true, false, false ->
      print_endline ("You have only one card to disprove the suggestion" ^
                     "and you have to reveal the card.");
      Some (Prof who)
  | false, true, true ->
      print_endline ("You have two cards to disprove the suggestion" ^
                     "and you have to reveal one of them.");
      choose_from_two (Building where) (Language with_what)
  | false, true, false ->
      print_endline ("You have only one card to disprove the suggestion" ^
                     "and you have to reveal the card.");
      Some (Building where)
  | false, false, true ->
      print_endline ("You have only one card to disprove the suggestion" ^
                     "and you have to reveal the card.");
      Some (Language with_what)
  | false, false, false ->
      print_endline "You do not have any cards to disprove the suggestion.";
      None

