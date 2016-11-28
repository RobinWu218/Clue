open Data
open Gmap
open Ai
open User


(******************************)
(* init_state and its helpers *)
(******************************)


(* [generate_case_file ()] is the case file containing the answers to the
questions: Who? Where? What language? *)
let generate_case_file () : case_file =
    let r_prof = prof_of_int(Random.int 6) in
    let r_building = building_of_int(Random.int 9) in
    let r_lang = lang_of_int(Random.int 6) in
    {who = r_prof; where = r_building; with_what = r_lang}

(* [select_non_repeat_lst] is an int list generated randomly given the size
 * and bound and the exclueded elements*)
let select_non_repeat_lst excluded_lst lst size bound =

    while (List.length !lst < size) do (
        let r =  Random.int bound in
        if (List.mem r !lst) || (List.mem r excluded_lst) then
          lst:= !lst
        else
          lst:= (r::!lst)
    )done

(* [assign_characters n] is a list of non-repeating profs.
 * Requires: [n] is an integer between 3 and 6 inclusive.
 *           [prof_chosen] is a string list indicating the chosen professor*)
let assign_characters prof_chosen (n:int) =
    let char_lst = ref [] in
    select_non_repeat_lst (prof_lst_to_int_lst prof_chosen) char_lst n 6;
    lst_to_prof_lst (!char_lst)

(* [choose_card n] is an list of non-repeating cards.
 * Requires: [n] is an integer between 3 and 6 inclusive. 
 *           [card_chosen] is a card list indicating the chosen cards*)
let choose_card card_chosen (n:int) =
    let card_lst = ref [] in
    select_non_repeat_lst (card_lst_to_int_lst card_chosen) card_lst n 21;
    int_lst_to_card_lst (!card_lst)

let deal_card fact_card n = 
  match n with 
  | 2 -> 
    let lst1 = choose_card fact_card 6 in
    let lst2 = choose_card (fact_card @ lst1) 6 in 
    let lst3 = choose_card (fact_card @ lst1 @ lst2) 6 in 
    [lst1;lst2;lst3]
  | 3 -> 
    let lst1 = choose_card fact_card 5 in
    let lst2 = choose_card (fact_card @ lst1) 5 in 
    let lst3 = choose_card (fact_card @ lst1 @ lst2) 4 in 
    let lst4 = choose_card (fact_card @ lst1 @ lst2 @ lst3) 4 in 
    [lst1; lst2; lst3; lst4]
  | 4 ->
    let lst1 = choose_card fact_card 4 in
    let lst2 = choose_card (fact_card @ lst1) 4 in 
    let lst3 = choose_card (fact_card @ lst1 @ lst2) 4 in 
    let lst4 = choose_card (fact_card @ lst1 @ lst2 @ lst3) 3 in
    let lst5 = choose_card (fact_card @ lst1 @ lst2 @ lst3 @ lst4) 3 in
    [lst1; lst2; lst3; lst4; lst5]
  | 5 ->
    let lst1 = choose_card fact_card 3 in
    let lst2 = choose_card (fact_card @ lst1) 3 in 
    let lst3 = choose_card (fact_card @ lst1 @ lst2) 3 in 
    let lst4 = choose_card (fact_card @ lst1 @ lst2 @ lst3) 3 in
    let lst5 = choose_card (fact_card @ lst1 @ lst2 @ lst3 @ lst4) 3 in
    let lst6 = choose_card (fact_card @ lst1 @ lst2 @ lst3 @ lst4 @ lst5) 3 in
    [lst1; lst2; lst3; lst4; lst5; lst6]

let remove_first_el lst = 
  match lst with 
  | [] -> failwith "No elements in the list"
  | h::t -> t

let init_ai_lst n d hand_lst character_lst = 
  let ai_lst = ref [] in 
  while (List.length !ai_lst < n) do (
    let nth = List.length !ai_lst in 
    let character = List.nth character_lst nth in 
    let hand = List.nth hand_lst nth in 
    let ai = AI.init character d hand in 
    ai_lst := (!ai_lst) @ [ai]
  ) done;
  !ai_lst

let rec generate_dictionary prof_lst user_char ai_char_lst = 
  match prof_lst with 
  | [] -> []
  | h::t -> 
    if (h = user_char) then 
      (h, `User) :: generate_dictionary t user_char ai_lst 
    else if (List.mem h ai_char_lst) then 
      (h, `AI) :: generate_dictionary t user_char ai_lst 
    else 
      (h, `No) :: generate_dictionary t user_char ai_lst 

(* [init n d] is the initial game state with [n] AI bots and a difficulty level
of [d]. It prints out which character each player plays, and the cards in the
user’s hands.
 * Requires: [n] is an integer between 2 and 5 inclusive, [d] is an integer
between 1 and 3 inclusive. *)
let init_state (n:int) (d:int) : state =
  if (n >= 2) && (n <= 5) then
    let fact_file = generate_case_file () in 
    let fact_cards = [Prof fact_file.who; Building fact_file.where; Language fact_file.with_what];
    let character_lst = assign_characters (fact_file.who) n in
    let dealt_cards_lst = deal_card fact_cards n in 
    let user_hand = List.hd dealt_cards_lst in 
    let ai_hand_lst = remove_first_el dealt_cards_lst in 
    let user_character = List.hd character_lst in 
    let ai_characters_lst = remove_first_el character_lst in 
    let ai_lst = init_ai_lst n d ai_hand_lst character_lst in 
    let dictionary = generate_dictionary 
                      ["Bracy";"Clarkson";"Fan";"Gries";"Halpern";"White"] 
                      user_character ai_characters_lst in 
    {counter = 0;
     game_complete = false;
     map = make_map();
     user = {character = user_character; hand = user_hand; was_moved = false};
     ais = ai_lst;
     fact_file = fact_file;
     dictionary = dictionary
     }
    else failwith "This should not happen in init_state"

(************************)
(* repl and its helpers *)
(************************)

(* [roll_two_dice ()] is the sum of two random integers between 1 and 6 inclusive. It also prints the two integers and the sum. *)
let roll_two_dice () =
    let d1 = 1 + Random.int 5 in
    let d2 = 1 + Random.int 5 in
    let sum = d1 + d2 in
    print_endline "Die 1: " ^ (string_of_int d1);
    print_endline "Die 2: " ^ (string_of_int d2);
    print_endline "# of movements: " ^ (string_of_int sum);

(* [update_state c s] is the new state after command [c] is executed when
 * the current state is [s]. *)
val update_state = failwith "TODO"

(* [repl turn s] is . *)
let rec repl (turn:int) (s:state) : unit =
    if (turn mod s.n) = 0 then ...
    else if (turn mod s.n) = 1 then ...
    else if (turn mod s.n) = 2 then ...
    ...
    repl (turn + 1) s'

(********)
(* main *)
(********)

(* [main n d] is the main entry point from outside this module to initialize a game with [n] AI bots and a difficulty level of [d] and start playing it. *)
let main n d =
  let s = init_state n d in
  print_endline "The game begins now... ";
  print_endline "To play the game, type short phrases into the command line when
   it is your turn. You may terminate the game at any time with [quit], show the
    current map with [map], show your cards with [cards], and redisplay
    instructions with [help]. \n";
  repl 0 s