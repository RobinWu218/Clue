open Data
open Gmap

(********************)
(* helper functions *)
(********************)

(* [int_option_of_string s] is [Some i] if [s] can be converted to int [i]
 * using [int_of_string s], and [None] otherwise. *)
let int_option_of_string (s:string) : int option =
  try Some (int_of_string s)
  with Failure _ -> None

(* [get_choice ()] is [1] if the user selects the first choice and [2] if
 * the user selects the second choice. *)
let rec get_choice_two () : int =
  let str = print_string  "> "; read_line () in
  let str' = String.(str |> trim) in
  if String.length str' = 0 then
    print_endline "Please at least type something!"; get_choice_two ()
  else 
    match str'.[0] with 
    | '1' -> 1
    | '2' -> 2
    | _   -> print_endline "Please type either 1 or 2!"; get_choice_two ()

(* [get_choice_three ()] is [1] if the user selects the first choice, [2] if
 * the user selects the second choice, and [3] if the third. *)
let rec get_choice_three () : int =
  let str = print_string  "> "; read_line () in
  let str' = String.(str |> trim) in
  if String.length str' = 0 then
    print_endline "Please at least type something!"; get_choice_three ()
  else 
    match str'.[0] with 
    | '1' -> 1
    | '2' -> 2
    | '1' -> 3
    | _   -> print_endline "Please type 1 or 2 or 3!"; get_choice_three ()

(* [choose_from_two c1 c2] is [Some c1] or [Some c2] as determined by user. *)
let choose_from_two (c1:card) (c2:card) : card option =
  Printf.printf "You can reveal either card 1: %s, or card 2: %s. [1/2]\n"
                (string_of_card c1) (string_of_card c2);
  match get_choice_two () with 
  | 1 -> Some c1
  | 2 -> Some c2
  | _ -> failwith "This should not happen in choose_from_two in user.ml"

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
    | _   -> print_endline "Invalid input; try again please."; get_who ()

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
    | _   -> print_endline "Invalid input; try again please."; get_where ()

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
    | _   -> print_endline "Invalid input; try again please."; get_with_what ()

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
  | _   -> print_endline "Invalid command; try again please."; accuse_or_not s

(* [disprove_helper n guess s] is [Some (prof, card)] if [prof] disproved 
 * [guess] with [card] and [None] if no one can disprove [guess]. 
 * It starts with the professor corresponding to integer [n], goes along 
 * the loop B->C->F->G->H->W->B until someone is able to disprove [guess] 
 * or when the user's character is reached. *)
let rec disprove_helper (n:int) (guess:case_file) (s:state) 
                        : ((prof * card) option) =
  let n' = n mod 6 in
  if n' = int_of_card (Prof s.user.character) then None else
  match n' with
  | 0 -> disprove_helper_case "Bracy"    0 guess s
  | 1 -> disprove_helper_case "Clarkson" 1 guess s
  | 2 -> disprove_helper_case "Fan"      2 guess s
  | 3 -> disprove_helper_case "Gries"    3 guess s
  | 4 -> disprove_helper_case "Halpern"  4 guess s
  | 5 -> disprove_helper_case "White"    5 guess s
  | _ -> failwith "This should not happen in disprove_helper in user.ml"
(* [disprove_helper_case p n guess s] checks if the professor corresponding 
 * to integer [n] is represented by any ai and if so, if that ai can disprove 
 * [guess], before possibly calling [disprove_helper (n+1) guess s] to move 
 * on to check the next professor. *)
and disprove_helper_case (p:prof) (n:int) (guess:case_file) (s:state) 
                         : ((prof * card) option) =
  match List.assoc p s.dictionary with
  | `AI -> 
      let ai = List.find (fun a -> a.character = p) s.ais in
      begin
      match Ai.ai_disprove ai guess with_what
      | Some card -> Some (p, card)
      | None -> disprove_helper (n+1) guess s
      end
  | `User -> 
      failwith "This should not happen in disprove_helper_case in user.ml"
  | `None -> 
      disprove_helper (n+1) guess s

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
      let news = {s with game_complete = true; map = map} in
      let news' = assign_was_moved news who moved_or_not in
      let guess = 
        {who = Prof who; 
         where = Building where; 
         with_what = Language with_what} in
      let nuser = int_of_card (Prof s.user.character) in
      begin
      match disprove_helper (n+1) guess s with
      | Some (p, c) -> 
          Printf.printf "Prof. %s disproved your suggestion with card %s." 
                        p (string_of_card c);
          let news'' = 
            {news' with past_guesses = 
              (guess, s.user.character, Some prof)} in
          accuse_or_not news''
      | None -> 
          print_endline "No one can disprove your suggestion."; 
          let news'' = 
            {news' with past_guesses = 
              (guess, s.user.character, None)} in
          accuse_or_not news''
      end
  | None -> failwith "This should not happen in user_suggest in user.ml"

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
        | _  , Some x' when ((x' > n) || (x' <= 0)) ->         
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

(* [suggest_or_secret b s] prompts the user to choose between making a 
 * suggestion and using the secret passage to enter building [b], and returns 
 * the updated state. *)
let suggest_or_secret (b:building) (s:state) : state =
  Printf.printf ("You can either 1 make a suggestion now or 2 use the " ^
                 "secret passage to get into %s Hall. [1/2]\n") b;
  match get_choice_two () with 
  | 1 -> user_suggest s
  | 2 -> use_secret b s
  | _ -> failwith "This should not happen in suggest_or_secret in user.ml"

(* [secret_or_roll b s] prompts the user to choose between using the secret 
 * passage to enter building [b] and rolling the dice to move out, and returns 
 * the updated state. *)
let secret_or_roll (b:building) (s:state) : state =
  Printf.printf ("You can either 1 use the secret passage to get into %s " ^
                 "Hall or 2 roll the dice and move out. [1/2]\n") b;
  match get_choice_two () with 
  | 1 -> use_secret b s
  | 2 -> user_move (roll_two_dice ()) s
  | _ -> failwith "This should not happen in secret_or_roll in user.ml"

(* [suggest_or_roll s] prompts the user to choose between making a suggestion
 * and rolling the dice to move out, and returns the updated state. *)
let suggest_or_roll (s:state) : state =
  Printf.printf ("You can either 1 make a suggestion now or 2 roll the " ^
                 "dice and move out. [1/2]\n");
  match get_choice_two () with 
  | 1 -> user_suggest s
  | 2 -> user_move (roll_two_dice ()) s
  | _ -> failwith "This should not happen in suggest_or_roll in user.ml"

(* [secret_or_roll_or_suggest s] prompts the user to choose to use the secret
 * passage, or roll the dice to move out, or make a suggestion, and returns
 * the updated state. *)
let secret_or_roll_or_suggest (b:building) (s:state) : state =
  Printf.printf ("You can either 1 make a suggestion now, or 2 roll the " ^
                 "dice and move out, or 3 use the secret passage to get " ^
                 "into %b Hall. [1/2/3]\n") b;
  match get_choice_three () with 
  | 1 -> user_suggest s
  | 2 -> user_move (roll_two_dice ()) s
  | 3 -> use_secret b s
  | _ -> failwith "This should not happen in secret_or_roll_or_suggest in user"

(* [in_building_involuntarily b s] checks whether there is a secrect passage
 * in user's current building [b] and whether exits to [b] are all blocked, 
 * determines what the user can do given that the user was moved to the 
 * building by someone else, and returns the updated state accordingly. *)
let in_building_involuntarily (b:building) (s:state) : state =
  let secret = has_secret_passage s.map b in
  let blocked = is_building_blocked s.map b in
  let news = 
    begin
    match secret, blocked with
    | true,  true  -> 
        print_endline "There is a secret passage available.";
        print_endline "All exits to the current building are blocked."; 
        suggest_or_secret b s
    | true,  false -> 
        print_endline "There is a secret passage available.";
        secret_or_roll_or_suggest b s
    | false, true  -> 
        print_endline "All exits to the current building are blocked."; 
        user_suggest s
    | false, false -> 
        suggest_or_roll s
    end
  in assign_was_moved news s.user.character false

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
      secret_or_roll b s
  | false, true  -> 
      print_endline "There is no secret passage available.";
      print_endline "All exits to the current building are blocked."; 
      print_endline "You have to wait until your next turn.";
      s
  | false, false -> 
      user_move (roll_two_dice ()) s

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

