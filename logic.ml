open Data
open Gmap

(* [assign_was_moved s p b] assigns bool [b] to the [was_moved] field of
 * whoever playing the character of prof [p] in state [s]. If no one plays that
 * character, then [s] is simply unchanged. *)
let assign_was_moved (s:state) (p:prof) (b:bool) : state =
  match List.assoc p s.dictionary with
  | `AI -> 
      let newais = List.map 
        (fun a -> if a.character = p 
                  then {a with was_moved = b} 
                  else a) s.ais in
        {s with ais = newais}
  | `User -> 
      let newuser = {s.user with was_moved = b} in
        {s with user = newuser}
  | `No -> s

(* [roll_two_dice ()] simulates rolling two dice, prints the results, and 
 * returns the sum. *)
let roll_two_dice () : int =
  let d1 = 1 + Random.int 5 in (*same every game? TODO*)
  let d2 = 1 + Random.int 5 in
  let sum = d1 + d2 in
    print_endline "Rolling two dice...";
    Printf.printf "Die 1: %d\n" d1;
    Printf.printf "Die 2: %d\n" d2;
    sum

(* [get_choice_two ()] is [1] if the user selects the first choice and [2] if
 * the user selects the second choice. *)
let rec get_choice_two () : int =
  let str = print_string  "> "; read_line () in
  let str' = String.(str |> trim) in
  if String.length str' = 0 
  then
    begin
    print_endline "Please at least type something!"; 
    get_choice_two ()
    end
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
  if String.length str' = 0 
  then
    begin
    print_endline "Please at least type something!"; 
    get_choice_three ()
    end
  else 
    match str'.[0] with 
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | _   -> print_endline "Please type 1 or 2 or 3!"; get_choice_three ()

(* [get_choice_four ()] is [1] if the user selects the first choice, [2] if
 * the second, [3] if the third, and [4] if the fourth. *)
let rec get_choice_four () : int =
  let str = print_string  "> "; read_line () in
  let str' = String.(str |> trim) in
  if String.length str' = 0 
  then
    begin
    print_endline "Please at least type something!"; 
    get_choice_four ()
    end
  else 
    match str'.[0] with 
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | _   -> print_endline "Please type 1 or 2 or 3 or 4!"; get_choice_four ()

(* [get_choice_num_ai ()] is [2] up till [5] if the user chooses to play with 
 * [2] to [5] AI's. *)
let rec get_choice_num_ai () : int =
  let str = print_string  "> "; read_line () in
  let str' = String.(str |> trim) in
  if String.length str' = 0 
  then
    begin
    print_endline "Please at least type something!"; 
    get_choice_num_ai ()
    end
  else 
    match str'.[0] with 
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | _   -> print_endline "Please type an integer between 2 and 5 inclusive!"; 
             get_choice_num_ai ()

(* [user_choose_from_two c1 c2] is [Some c1] or [Some c2] as determined by 
 * user. *)
let user_choose_from_two (c1:card) (c2:card) : card option =
  Printf.printf "You can reveal either card 1: %s, or card 2: %s. [1/2]\n"
                (string_of_card c1) (string_of_card c2);
  match get_choice_two () with 
  | 1 -> Some c1
  | 2 -> Some c2
  | _ -> failwith "This should not happen in user_choose_from_two in Logic"

(* [user_choose_from_three c1 c2 c3] is [Some c1] or [Some c2] or [Some c3] as 
 * determined by user. *)
let user_choose_from_three (c1:card) (c2:card) (c3:card) : card option =
  print_endline "You can reveal one of the three cards: \n";
  Printf.printf "card 1: %s, card 2: %s, or card 3: %s. [1/2/3]\n"
                (string_of_card c1) (string_of_card c2) (string_of_card c3);
  match get_choice_three () with 
  | 1 -> Some c1
  | 2 -> Some c2
  | 3 -> Some c3
  | _ -> failwith "This should not happen in user_choose_from_three in Logic"

(*
(* [easy_helper_disprove hand guess] attempts to disprove [guess] with the cards
they have in their [hand]. Returns Some Card that the player uses to disprove
or None if no such card exists. *)
let rec easy_helper_disprove (hand:hand) (guess:case_file) : card option = 
  match hand with
  | []   -> None
  | h::t -> if (card_to_string h) = guess.who   ||
               (card_to_string h) = guess.where ||
               (card_to_string h) = guess.with_what 
            then (Some h) 
            else easy_helper_disprove t guess

(* [ai_disprove ai guess] figures out which card to reveal in response
 * to a suggestion [guess].
 * Returns: [Some c] where [c] is a card that [ai] can reveal. Or, if [ai] has
 * none of the cards in [guess], then it will return [None].
 *)
let ai_disprove (ai:ai) (guess: case_file) : card option =
  match ai.difficulty with
  | Easy   -> easy_helper_disprove ai.hand guess
  | Medium -> failwith "TODO"
  | Hard   -> failwith "TODO"
  (* TODO if one of the cards has already been in a past guess, the ai wants to
   show that one so we give the other players as little information as possible.
    *)
*)

(*TODO
 * AI logic:
 *   - Easy: simply chooses the first card. *)
let ai_choose_from_two (a:ai) (c1:card) (c2:card) : card option =
  match a.difficulty with
  | Easy   -> Some c1
  | Medium -> failwith "TODO"
  | Hard   -> failwith "TODO"

(*TODO
 * AI logic:
 *   - Easy: simply chooses the first card. *)
let ai_choose_from_three (a:ai) (c1:card) (c2:card) (c3:card) : card option =
  match a.difficulty with
  | Easy   -> Some c1
  | Medium -> failwith "TODO"
  | Hard   -> failwith "TODO"

(*TODO*)
let ai_disprove (a:ai) (guess:case_file) : card option =
  Printf.printf "It is Prof. %s's turn to disprove the suggestion:\n"
                a.character;
  let hand = a.hand in
  let {who; where; with_what} = guess in
  let who_or_not = List.mem (Prof who) hand in
  let where_or_not = List.mem (Building where) hand in
  let with_what_or_not = List.mem (Language with_what) hand in
  match who_or_not, where_or_not, with_what_or_not with
  | true, true, true ->
      Printf.printf "Prof. %s disproved the suggestion.\n" a.character;
      ai_choose_from_three a (Prof who) (Building where) (Language with_what)
  | true, true, false ->
      Printf.printf "Prof. %s disproved the suggestion.\n" a.character;
      ai_choose_from_two a (Prof who) (Building where)
  | true, false, true ->
      Printf.printf "Prof. %s disproved the suggestion.\n" a.character;
      ai_choose_from_two a (Prof who) (Language with_what)
  | true, false, false ->
      Printf.printf "Prof. %s disproved the suggestion.\n" a.character;
      Some (Prof who)
  | false, true, true ->
      Printf.printf "Prof. %s disproved the suggestion.\n" a.character;
      ai_choose_from_two a (Building where) (Language with_what)
  | false, true, false ->
      Printf.printf "Prof. %s disproved the suggestion.\n" a.character;
      Some (Building where)
  | false, false, true ->
      Printf.printf "Prof. %s disproved the suggestion.\n" a.character;
      Some (Language with_what)
  | false, false, false ->
      Printf.printf "Prof. %s was not able to disprove the suggestion.\n"
                    a.character;
      None

(* [user_disprove s guess] is [None] if the user does not have any card
 * to disprove the suggestion [guess] and a card option if the user has the 
 * card(s) and wishes to disprove [guess] with that card. *)
let user_disprove (s:state) (guess:case_file) : card option =
  print_endline "It is your turn to disprove the suggstion:";
  let hand = s.user.hand in
  let {who; where; with_what} = guess in
  let who_or_not       = List.mem (Prof who) hand in
  let where_or_not     = List.mem (Building where) hand in
  let with_what_or_not = List.mem (Language with_what) hand in
  match who_or_not, where_or_not, with_what_or_not with
  | true, true, true ->
      print_endline ("You have three cards to disprove the suggestion \n" ^
                     "and you have to reveal one of them.");
      user_choose_from_three (Prof who) (Building where) (Language with_what)
  | true, true, false ->
      print_endline ("You have two cards to disprove the suggestion \n" ^
                     "and you have to reveal one of them.");
      user_choose_from_two (Prof who) (Building where)
  | true, false, true ->
      print_endline ("You have two cards to disprove the suggestion \n" ^
                     "and you have to reveal one of them.");
      user_choose_from_two (Prof who) (Language with_what)
  | true, false, false ->
      print_endline ("You have only one card to disprove the suggestion \n" ^
                     "and you have to reveal the card.");
      Some (Prof who)
  | false, true, true ->
      print_endline ("You have two cards to disprove the suggestion \n" ^
                     "and you have to reveal one of them.");
      user_choose_from_two (Building where) (Language with_what)
  | false, true, false ->
      print_endline ("You have only one card to disprove the suggestion \n" ^
                     "and you have to reveal the card.");
      Some (Building where)
  | false, false, true ->
      print_endline ("You have only one card to disprove the suggestion \n" ^
                     "and you have to reveal the card.");
      Some (Language with_what)
  | false, false, false ->
      print_endline "You do not have any cards to disprove the suggestion.";
      None

