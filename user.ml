open Data
open Gmap
open Logic

(* [get_who ()] prompts the user for the professor s/he wants to suggest
 * or accuse and returns the corresponding string. *)
let rec get_who () : string =
  print_insn "Who did it? [Bracy/Clarkson/Fan/Gries/Halpern/White]" true;
  let str = print_string "> "; read_line () in
  let str' = String.(str |> trim |> lowercase_ascii) in
  if String.length str' = 0
  then
    begin
      print_insn "Please at least type something!" true;
      get_who ()
    end
  else
    match str' with
    | "bracy"    -> "Bracy"
    | "clarkson" -> "Clarkson"
    | "fan"      -> "Fan"
    | "gries"    -> "Gries"
    | "halpern"  -> "Halpern"
    | "white"    -> "White"
    | _   ->
      print_insn "Invalid input; please try again." true;
      get_who ()

(* [get_where ()] prompts the user for the builidng s/he wants to accuse
 * and returns the corresponding string. *)
let rec get_where () : string =
  print_insn ("Where? [Baker/Carpenter/Duffield/Gates/Klarman/Olin/"^
             "Phillips/Rhodes/Statler]") true;
  let str = print_string "> "; read_line () in
  let str' = String.(str |> trim |> lowercase_ascii) in
  if String.length str' = 0 then
    begin
      print_insn "Please at least type something!" true;
      get_where ()
    end
  else
    match str' with
    | "bakr" | "baker"     -> "Baker"
    | "carp" | "carpenter" -> "Carpenter"
    | "duff" | "duffield"  -> "Duffield"
    | "gate" | "gates"     -> "Gates"
    | "klrn" | "klarman"   -> "Klarman"
    | "olin"               -> "Olin"
    | "phil" | "phillips"  -> "Phillips"
    | "rhod" | "rhodes"    -> "Rhodes"
    | "stat" | "statler"   -> "Statler"
    | _   ->
      print_insn "Invalid input; please try again." true;
      get_where ()

(* [get_with_what ()] prompts the user for the language s/he wants to suggest
 * or accuse and returns the corresponding string. *)
let rec get_with_what () : string =
  print_insn "With what language? [Bash/C/Java/MATLAB/OCaml/Python]" true;
  let str = print_string "> "; read_line () in
  let str' = String.(str |> trim |> lowercase_ascii) in
  if String.length str' = 0
  then
    begin
      print_insn "Please at least type something!" true;
      get_with_what ()
    end
  else
    match str' with
    | "bash"   -> "Bash"
    | "c"      -> "C"
    | "java"   -> "Java"
    | "matlab" -> "MATLAB"
    | "ocaml"  -> "OCaml"
    | "python" -> "Python"
    | _   ->
      print_insn "Invalid input; please try again." true;
      get_with_what ()

(* [accuse s] prompts the user for his/her accusation, determines whether
 * the user wins or not, and ends the game by returning the updated state.
 * Calls [teleport_professor] to move the suggested prof's corresponding ai
 * player to the suggested building and change that ai's was_moved field to
 * true if ai is not already in the building before being moved. *)
let accuse (s:state) : state =
  let who       = get_who () in
  let where     = get_where () in
  let with_what = get_with_what () in
  let (moved_or_not, map) =
    if get_current_building s.map who <> (Some where) 
    then (true, (teleport_professor s.map who where)) 
    else (false, s.map) in
  let accusation =
    { who       = who;
      where     = where;
      with_what = with_what } in
  print_results "Your accusation is: " true;
  print_case_file accusation;
  wait_for_user();
  print_results "The fact file is..." true;
  print_case_file s.fact_file;
  if accusation = s.fact_file
  then
    begin
      print_important "You were correct! Awesome! You won!" true;
      print_win ();
      let news = {s with game_complete = true; map = map} in
        assign_was_moved news who moved_or_not 
    end
  else
    begin
      print_important "Oh no! Your accusation was incorrect." true;
      print_important "Unfortunately, you have just LOST the game. :(" true;
      print_info "CLUE will exit automatically. Feel free to play again!" true;
    let news = {s with game_complete = true; map = map} in
    assign_was_moved news who moved_or_not
    end

(* [accuse_or_not s] asks the user whether s/he wants to make an accusation or
 * not, and if so, updates the current state [s] by calling [accuse s]. *)
let rec accuse_or_not (s:state) : state =
  print_insn "Do you want to accuse? [y/n]" true;
  let str = print_string "> "; read_line () in
  match String.(str |> trim |> lowercase_ascii) with
  | "y" -> accuse s
  | "n" -> s
  | _   ->
      print_insn "Invalid command; please try again." true;
      accuse_or_not s

(* [disprove_loop n guess s] is [Some (prof, card)] if [prof] disproved
 * [guess] with [card] and [None] if no one can disprove [guess].
 * It starts with the professor corresponding to integer [n], goes along
 * the loop B->C->F->G->H->W->B until someone is able to disprove [guess]
 * or when the user's character is reached. *)
let rec disprove_loop (n:int)(guess:case_file)(s:state):((prof * card) option)=
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
          match ai_disprove ai guess s.ais with (* Logic *)
          | Some card -> Some (p, card)
          | None      -> disprove_loop (n+1) guess s
        end
  | `User ->
      failwith "This should not happen in disprove_case in user.ml"
  | `No ->
      disprove_loop (n+1) guess s

(* [suggest s] prompts the user for his/her suggestion and calls
 * [Ai.disprove] until the user is disproved or not disproved in
 * the end. Calls [teleport_professor] to move the suggested prof's 
 * corresponding ai player to the suggested building and change 
 * that ai's [was_moved] field to true.
 * Requires: user is currently in a building. *)
let suggest (s:state) : state =
  print_insn "Please make a suggestion about the current building now." true;
  let who = get_who () in
  let where_option = get_current_building s.map s.user.character in
  let with_what = get_with_what () in
  match where_option with
  | Some where ->
      let guess =
        {who = who;
         where = where;
         with_what = with_what} in
      print_results " " true;
      print_results "Your suggestion is: " true;
      print_case_file guess;
      let (moved_or_not, map) =
        if get_current_building s.map who <> (Some where) 
        then (true, (teleport_professor s.map who where)) 
        else (false, s.map) in
      let news  = {s with map = map} in
      let news' = assign_was_moved news who moved_or_not in 
      let nuser = int_of_card (Prof s.user.character) in
      begin
        match disprove_loop (nuser+1) guess s with
        | Some (p, c) ->
          begin
            ANSITerminal.(
              print_info ("Professor "^p^" showed you the card: ") true;
              print_string []
                (sprintf card_style "  %-68s" (string_of_card c));
              print_endline "";
              );
            let news'' =
              {news' with past_guesses =
              (guess, s.user.character, Some p) >:: news'.past_guesses} in
              accuse_or_not news''
          end
        | None ->
          begin
              print_info "No one could disprove your suggestion." true;          
            let news'' =
              {news' with past_guesses =
              (guess, s.user.character, None) >:: news'.past_guesses} in
              accuse_or_not news''
          end
      end
  | None -> failwith "This should not happen in suggest in user.ml"

(* [get_movement n] prompts the user for the next steps s/he wants to take
 * and, if feasible, returns the corresponding tuple representation of the
 * desired movement. *)
let rec get_movement (n:int) : string * int =
  print_insn ( 
    "You can move "^(string_of_int n)^
    " steps in directions [up/down/left/right].")
    true;
  print_info (
    "Enter commands for one direction at a time. "^
    "e.g: up 2, right 4, etc.")
    true;
  let str = print_string "> "; read_line () |> String.lowercase_ascii in
  let lst = Str.(str |> split (regexp "[ ]+")) in
  match lst with
  | dir::xstr::[] ->
      let x = int_option_of_string xstr in (* Data *)
      begin
        match dir.[0], x with
        | _  , Some x' when ((x' > n) || (x' <= 0)) ->
          print_insn "Invalid number of steps; please try again." true;
          get_movement n
        | 'u', Some x' -> ("up",    x')
        | 'd', Some x' -> ("down",  x')
        | 'l', Some x' -> ("left",  x')
        | 'r', Some x' -> ("right", x')
        | _ -> 
          print_insn "Invalid input; please try again." true;
          get_movement n
      end
  | _ -> print_insn "Invalid input; please try again." true; 
         get_movement n

(* [move n bop s] prompts the user to enter commands so that his/her
 * character moves n steps, or fewer if the character gets into a building
 * before using up all the steps. If [bop] is [Some b'] then user cannot
 * move into [b'] since s/he just left that building in the same turn. *)
let rec move (n:int) (bop:building option) (s:state) : state =
  if n < 0 then
    failwith "This should not happen in move"
  else if n = 0 then
    begin
      print_results "You cannot move anymore." true;
      s
    end
  else
    let (dir, x) = get_movement n in
    let (y, map) = Gmap.move s.map s.user.character bop dir x in
      if is_in_building map s.user.character
      then suggest {s with map = map}
      else move (n-x+y) bop {s with map = map}

(* [get_exit b s] is the id of an exit to building [b] selected by the user. *)
let rec get_exit (b:building) (s:state) : int =
  let exits = List.assoc b s.map.exits in
  match List.length exits with
  | 1 -> 1
  | 2 -> 
    begin
        print_insn (
          "Please choose one of the two exits of "^b^" Hall to leave.")
          true;
        print_exits exits;
        print_insn "Valid responses are: [1/2]" true;
      let choice = get_choice_two () in
      if is_coord_blocked s.map (List.assoc choice exits)
      then 
        begin
          print_insn "This exit is blocked! Please pick another." true;
          get_exit b s
        end
      else choice
    end
  | 4 -> 
    begin
        print_insn (
          "Please choose one of the four exits of "^b^" Hall to leave.")
          true;
        print_exits exits;
        print_insn "Valid responses are: [1/2/3/4]" true;
      let choice = get_choice_four () in
      if is_coord_blocked s.map (List.assoc choice exits)
      then 
        begin
          print_insn "This exit is blocked! Please pick another." true;
          get_exit b s
        end
      else choice
    end
  | _ -> failwith "This should not happen in get_exit in User given map.json"

(* [leave_and_move b s] is the updated state after the user moves out of
 * building [b].
 * Requires: [s.user] is currently in building [b]. *)
let leave_and_move (b:building) (s:state) : state =
  let map = leave_building s.map s.user.character (get_exit b s) in
    move (roll_two_dice ()) (Some b) {s with map = map}

(* [use_secret s] is the updated state after the user uses the secret
 * passage in the current building.
 * Requires: [s.user] is currently in a building where there is a secret
 *           passage.  *)
let use_secret (s:state) : state =
  let map = use_secret_passage s.map s.user.character in
    suggest {s with map = map}

(* [suggest_or_secret b s] prompts the user to choose between making a
 * suggestion and using the secret passage to leave building [b], and returns
 * the updated state. *)
let suggest_or_secret (b:building) (s:state) : state =
    print_insn  "You can either:" true;
    print_insn  "   1) make a suggestion now or" true;
    print_insn ("   2) use the secret passage to get into "^
      (List.assoc b s.map.secrets)^" Hall.")
      true;
    print_insn "Valid responses are: [1/2]" true;
  match get_choice_two () with
    | 1 -> suggest s
    | 2 -> use_secret s
    | _ -> failwith "This should not happen in suggest_or_secret in user.ml"

(* [secret_or_roll b s] prompts the user to choose between using the secret
 * passage to leave building [b] and rolling the dice to move out, and returns
 * the updated state. *)
let secret_or_roll (b:building) (s:state) : state =
  print_insn  "You can either:" true;
  print_insn  "   1) roll the dice and move out, or" true;
  print_insn ("   2) use the secret passage to get into "^
    (List.assoc b s.map.secrets)^" Hall.")
    true;
  print_insn "Valid responses are: [1/2]" true;
  match get_choice_two () with
  | 1 -> leave_and_move b s
  | 2 -> use_secret s
  | _ -> failwith "This should not happen in secret_or_roll in user.ml"

(* [suggest_or_roll b s] prompts the user to choose between making a suggestion
 * and rolling the dice to move out, and returns the updated state. *)
let suggest_or_roll (b:building) (s:state) : state =
  print_insn "You can either:" true;
  print_insn "   1) make a suggestion now, or"   true;
  print_insn "   2) roll the dice and move out." true;
  print_insn "Valid responses are: [1/2]" true;
  match get_choice_two () with
  | 1 -> suggest s
  | 2 -> leave_and_move b s
  | _ -> failwith "This should not happen in suggest_or_roll in user.ml"

(* [secret_or_roll_or_suggest s] prompts the user to choose to use the secret
 * passage, or roll the dice to move out, or make a suggestion, and returns
 * the updated state. *)
let secret_or_roll_or_suggest (b:building) (s:state) : state =
  print_insn  "You can either:" true;
  print_insn  "   1) make a suggestion now, or"   true;
  print_insn  "   2) roll the dice and move out." true;
  print_insn ("   3) use the secret passage to get into "^
    (List.assoc b s.map.secrets)^" Hall.")
    true;
  print_insn "Valid responses are: [1/2/3]" true;
  match get_choice_three () with
  | 1 -> suggest s
  | 2 -> leave_and_move b s
  | 3 -> use_secret s
  | _ -> failwith "This should not happen in secret_or_roll_or_suggest in user"

(* [in_building_involuntarily b s] checks whether there is a secrect passage
 * in user's current building [b] and whether exits to [b] are all blocked,
 * determines what the user can do given that the user was moved to the
 * building by someone else, and returns the updated state accordingly. *)
let in_building_involuntarily (b:building) (s:state) : state =
  let secret  = has_secret_passage  s.map b in
  let blocked = is_building_blocked s.map b in
  let news =
    begin
      match secret, blocked with
      | true,  true  ->
        print_info "There is a secret passage available..." true;
        print_info "All other exits to the current building are blocked." true;
        suggest_or_secret b s
      | true,  false ->
        print_info "There is a secret passage available..." true;
        secret_or_roll_or_suggest b s
      | false, true  ->
        print_info "All exits to the current building are blocked." true;
        suggest s
      | false, false ->
        suggest_or_roll b s
    end
  in assign_was_moved news s.user.character false

(* [in_building_voluntarily b s] checks whether there is a secrect passage
 * in user's current building [b] and whether exits to [b] are all blocked,
 * determines what the user can do given that the user entered the building
 * on his/her own, and returns the updated state accordingly. *)
let in_building_voluntarily (b:building) (s:state) : state =
  let secret  = has_secret_passage  s.map b in 
  let blocked = is_building_blocked s.map b in 
  match secret, blocked with
  | true,  true  ->
    print_info "All exits to the current building are blocked." true;
    print_info "   --> You have to use the secret passage!" true;
    use_secret s
  | true,  false ->
    print_info "There is a secret passage available..." true;
    secret_or_roll b s
  | false, true  ->
    print_info "All exits to the current building are blocked." true;
    print_info "   --> You have to wait until your next turn!" true;
    s
  | false, false ->
    leave_and_move b s

(* [step s] is the new state after the user finishes his/her turn when
 * the current state is [s]. *)
let step (s:state) : state =
  let s1 = accuse_or_not s in
  if s1.game_complete then s1 else
  match get_current_building s1.map s1.user.character with
  | Some b ->
      if s1.user.was_moved
      then in_building_involuntarily b s1
      else in_building_voluntarily b s1
  | None ->
      let c = get_current_location s1.map s1.user.character in
      if is_coord_blocked s1.map c then s1 else
      begin
        print_map s.map;
        move (roll_two_dice ()) None s1  
      end
      