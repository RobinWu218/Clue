open Data
open Gmap
open Logic

(********************)
(* helper functions *)
(********************)

(* [get_who ()] prompts the user for the professor s/he wants to suggest
 * or accuse and returns the corresponding string. *)
let rec get_who () : string =
  print_endline "Who did it? [Bracy/Clarkson/Fan/Gries/Halpern/White]";
  let str = print_string  "> "; read_line () in
  let str' = String.(str |> trim |> lowercase_ascii) in
  if String.length str' = 0 
  then
    begin
    print_endline "Please at least type something!"; 
    get_who ()
    end
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
    begin
    print_endline "Please at least type something!"; 
    get_where ()
    end
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
  if String.length str' = 0 
  then
    begin
    print_endline "Please at least type something!"; 
    get_with_what ()
    end
  else 
    match str'.[0] with 
    | 'b' -> "Bash"
    | 'c' -> "C"
    | 'j' -> "Java"
    | 'm' -> "MATLAB"
    | 'o' -> "OCaml"
    | 'p' -> "Python"
    | _   -> print_endline "Invalid input; try again please."; get_with_what ()

(* [accuse s] prompts the user for his/her accusation, determines whether 
 * the user wins or not, and ends the game by returning the updated state.  
 * Calls [teleport_professor] to move the suggested prof's corresponding ai 
 * player to the suggested building and change that ai's was_moved field to 
 * true if ai is not already in the building before being moved. *)
let accuse (s:state) : state =
  let who = get_who () in
  let where = get_where () in
  let with_what = get_with_what () in
  let (moved_or_not, map) = 
    if get_current_building s.map who <> (Some where) (* Gmap *)
    then (true, (teleport_professor s.map who where)) (* Gmap *)
    else (false, s.map) in
  let accusation = 
    {who = who; 
     where = where; 
     with_what = with_what} in
  if accusation = s.fact_file 
  then
    begin
    print_endline "Awesome! You got the right accusation.";
    print_endline "YOU WIN!!!";
    print_endline "CLUE will exit automatically. Feel free to play again!";
    let news = {s with game_complete = true; map = map} in
    assign_was_moved news who moved_or_not (* Gmap *)
    end
  else
    begin
    print_endline "Uh-oh, wrong accusation.";
    print_endline "Unfortunately, you have just lost the game. :(";
    print_endline "The real case file is:";
    Printf.printf "Prof. %s created the virus with %s in %s Hall.\n"
                  s.fact_file.who s.fact_file.with_what s.fact_file.where;
    print_endline "CLUE will exit automatically. Feel free to play again!";
    let news = {s with game_complete = true; map = map} in
    assign_was_moved news who moved_or_not (* Gmap *)
    end

(* [accuse_or_not s] asks the user whether s/he wants to make an accusation or
 * not, and if so, updates the current state [s] by calling [accuse s]. *)
let rec accuse_or_not (s:state) : state =
  print_endline "Do you want to accuse? [y/n]";
  let str = print_string  "> "; read_line () in
  match String.(str |> trim |> lowercase_ascii) with 
  | "y" -> accuse s
  | "n" -> s
  | _   -> print_endline "Invalid command; try again please."; accuse_or_not s

(* [disprove_loop n guess s] is [Some (prof, card)] if [prof] disproved 
 * [guess] with [card] and [None] if no one can disprove [guess]. 
 * It starts with the professor corresponding to integer [n], goes along 
 * the loop B->C->F->G->H->W->B until someone is able to disprove [guess] 
 * or when the user's character is reached. *)
let rec disprove_loop (n:int) (guess:case_file) (s:state) 
                        : ((prof * card) option) =
  let n' = n mod 6 in
  if n' = int_of_card (Prof s.user.character) then None else
  match n' with
  | 0 -> disprove_case "Bracy"    0 guess s
  | 1 -> disprove_case "Clarkson" 1 guess s
  | 2 -> disprove_case "Fan"      2 guess s
  | 3 -> disprove_case "Gries"    3 guess s
  | 4 -> disprove_case "Halpern"  4 guess s
  | 5 -> disprove_case "White"    5 guess s
  | _ -> failwith "This should not happen in disprove_loop in user.ml"
(* [disprove_case p n guess s] checks if the professor corresponding 
 * to integer [n] is represented by any ai and if so, if that ai can disprove 
 * [guess], before possibly calling [disprove_loop (n+1) guess s] to move 
 * on to check the next professor. *)
and disprove_case (p:prof) (n:int) (guess:case_file) (s:state) 
                         : ((prof * card) option) =
  match List.assoc p s.dictionary with
  | `AI -> 
      let ai = List.find (fun a -> a.character = p) s.ais in
      begin
      match ai_disprove ai guess with (* Logic *)
      | Some card -> Some (p, card)
      | None -> disprove_loop (n+1) guess s
      end
  | `User -> 
      failwith "This should not happen in disprove_case in user.ml"
  | `No -> 
      disprove_loop (n+1) guess s

(* [suggest s] prompts the user for his/her suggestion and calls 
 * [Ai.disprove] until it is disproved or all passed. Calls [teleport_professor]
 * to move the suggested prof's corresponding ai player to the suggested 
 * building and change that ai's [was_moved] field to true. 
 * Requires: user is currently in a building. *)
let suggest (s:state) : state =
  print_endline "Please make a suggestion about the current building now.";
  let who = get_who () in
  let where_option = get_current_building s.map s.user.character in (* Gmap *)
  let with_what = get_with_what () in
  match where_option with
  | Some where ->
      let (moved_or_not, map) = 
        if get_current_building s.map who <> (Some where) (* Gmap *)
        then (true, (teleport_professor s.map who where)) (* Gmap *)
        else (false, s.map) in
      let news = {s with map = map} in
      let news' = assign_was_moved news who moved_or_not in (* Gmap *)
      let guess = 
        {who = who; 
         where = where; 
         with_what = with_what} in
      let nuser = int_of_card (Prof s.user.character) in
      begin
      match disprove_loop (nuser+1) guess s with
      | Some (p, c) -> 
          Printf.printf "Prof. %s disproved your suggestion with card %s.\n" 
                        p (string_of_card c);
          let news'' = 
            {news' with past_guesses = (*TODO possibly have a helper.ml?*)
              (guess, s.user.character, Some p)::news'.past_guesses} in
          accuse_or_not news''
      | None -> 
          print_endline "No one could disprove your suggestion."; 
          let news'' = 
            {news' with past_guesses = 
              (guess, s.user.character, None)::news'.past_guesses} in
          accuse_or_not news''
      end
  | None -> failwith "This should not happen in suggest in user.ml"

(* [get_movement n] prompts the user for the next steps s/he wants to take
 * and, if feasible, returns the corresponding tuple representation of the
 * desired movement. *)
let rec get_movement (n:int) : string * int =
  Printf.printf "You can move %d steps... [up/down/left/right i]\n" n;
  let str = print_string  "> "; read_line () |> String.lowercase_ascii in
  let lst = Str.(str |> split (regexp "[ ]+")) in
  match lst with
  | dir::xstr::[] ->
      let x = int_option_of_string xstr in (* Data *)
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

(* [move n s] prompts the user to enter commands so that his/her 
 * character moves n steps, or fewer if the character gets into a building
 * before using up all the steps. *)
let rec move (n:int) (s:state) : state =
  if n < 0 then 
    failwith "This should not happen in move"
  else if is_in_building s.map s.user.character then 
    suggest s
  else if n = 0 then 
    begin
      print_endline "You cannot move anymore.";
      s
    end
  else
    let (dir, x) = get_movement n in
    let (y, map) = Gmap.move s.map s.user.character dir x in (* Gmap *)
    move (n-x+y) {s with map = map}

(* [use_secret s] is the updated state after the user uses the secret 
 * passage in the current building. 
 * Requires: s.user is currently in a building where there is a secret 
 *           passage. *)
let use_secret (s:state) : state =
  let map = use_secret_passage s.map s.user.character in (* Gmap *)
  suggest {s with map = map}

(* [suggest_or_secret b s] prompts the user to choose between making a 
 * suggestion and using the secret passage to enter building [b], and returns 
 * the updated state. *)
let suggest_or_secret (b:building) (s:state) : state =
  print_endline "You can either";
  print_endline "  1 make a suggestion now or ";
  Printf.printf "  2 use the secret passage to get into %s Hall.\n" b;
  print_endline "Valid responses are: [1/2]";
  match get_choice_two () with 
  | 1 -> suggest s
  | 2 -> use_secret s
  | _ -> failwith "This should not happen in suggest_or_secret in user.ml"

(* [secret_or_roll b s] prompts the user to choose between using the secret 
 * passage to enter building [b] and rolling the dice to move out, and returns 
 * the updated state. *)
let secret_or_roll (b:building) (s:state) : state =
  print_endline "You can either:";
  print_endline "  1 roll the dice and move out, or";
  Printf.printf "  2 use the secret passage to get into %s Hall.\n" b;
  print_endline "Valid responses are: [1/2]";
  match get_choice_two () with 
  | 1 -> move (roll_two_dice ()) s
  | 2 -> use_secret s
  | _ -> failwith "This should not happen in secret_or_roll in user.ml"

(* [suggest_or_roll s] prompts the user to choose between making a suggestion
 * and rolling the dice to move out, and returns the updated state. *)
let suggest_or_roll (s:state) : state =
  print_endline "You can either:";
  print_endline "  1 make a suggestion now, or";
  print_endline "  2 roll the dice and move out.";
  print_endline "Valid responses are: [1/2]";
  match get_choice_two () with 
  | 1 -> suggest s
  | 2 -> move (roll_two_dice ()) s
  | _ -> failwith "This should not happen in suggest_or_roll in user.ml"

(* [secret_or_roll_or_suggest s] prompts the user to choose to use the secret
 * passage, or roll the dice to move out, or make a suggestion, and returns
 * the updated state. *)
let secret_or_roll_or_suggest (b:building) (s:state) : state =
  print_endline "You can either:";
  print_endline "  1 make a suggestion now, or ";
  print_endline "  2 roll the dice and move out, or ";
  Printf.printf "  3 use the secret passage to get into %s Hall.\n" b;
  print_endline "Valid responses are: [1/2/3]";
  match get_choice_three () with 
  | 1 -> suggest s
  | 2 -> move (roll_two_dice ()) s
  | 3 -> use_secret s
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
        suggest s
    | false, false -> 
        suggest_or_roll s
    end
  in assign_was_moved news s.user.character false

(* [in_building_voluntarily b s] checks whether there is a secrect passage
 * in user's current building [b] and whether exits to [b] are all blocked, 
 * determines what the user can do given that the user entered the building
 * on his/her own, and returns the updated state accordingly. *)
let in_building_voluntarily (b:building) (s:state) : state =
  let secret = has_secret_passage s.map b in (* Gmap *)
  let blocked = is_building_blocked s.map b in (* Gmap *)
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
      move (roll_two_dice ()) s

(******************)
(* main functions *)
(******************)

(* [step s] is the new state after the user finishes his/her turn when
 * the current state is [s]. *)
let step (s:state) : state =
  let s1 = accuse_or_not s in
  if s1.game_complete then s1 else 
  match get_current_building s1.map s1.user.character with (* Gmap *)
  | Some b ->
      if s1.user.was_moved 
      then in_building_involuntarily b s1
      else in_building_voluntarily b s1
  | None ->
      move (roll_two_dice ()) s1 (* Gmap *)

