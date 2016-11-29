open Data
open Gmap

(******************************)
(* init_state and its helpers *)
(******************************)

(* [generate_case_file ()] is the case file containing the answers to the
 * questions: Who? Where? What language? *)
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
  int_lst_to_prof_lst (!char_lst)

(* [choose_card n] is an list of non-repeating cards.
 * Requires: [n] is an integer between 3 and 6 inclusive.
 *           [card_chosen] is a card list indicating the chosen cards*)
let choose_card card_chosen (n:int) =
  let card_lst = ref [] in
  select_non_repeat_lst (card_lst_to_int_lst card_chosen) card_lst n 21;
  int_lst_to_card_lst (!card_lst)

(*TODO*)
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
  | _ -> failwith "This shoudl not happen in deal_card in game.ml"

(*TODO*)
let remove_first_el lst =
  match lst with
  | [] -> failwith "No elements in the list"
  | h::t -> t

(*TODO*)
let init_ai_lst n d hand_lst character_lst =
  let ai_lst = ref [] in
  while (List.length !ai_lst < n) do (
    let nth = List.length !ai_lst in
    let character = List.nth character_lst nth in
    let hand = List.nth hand_lst nth in
    let ai = Ai.init character hand d in
    ai_lst := (!ai_lst) @ [ai]
  ) done;
  !ai_lst

(*TODO Robin double check? Alice changed ai_char_lst to ai_lst *)
let rec generate_dictionary prof_lst user_char ai_lst =
  match prof_lst with
  | [] -> []
  | h::t ->
    if (h = user_char) then
      (h, `User) :: generate_dictionary t user_char ai_lst
    else if (List.mem h ai_lst) then
      (h, `AI) :: generate_dictionary t user_char ai_lst
    else
      (h, `No) :: generate_dictionary t user_char ai_lst

(* [init n d] is the initial game state with [n] AI bots and a difficulty level
 * of [d]. It prints out which character each player plays, and the cards in
 * the user’s hands.
 * Requires: [n] is an integer between 2 and 5 inclusive, [d] is an integer
 * between 1 and 3 inclusive. *)
let init_state (n:int) (d:difficulty) : state =
  Random.self_init ();  
                  (*Fix the problem of producing same sequence of nums every
                   *time we launch utop*)
  if (n >= 2) && (n <= 5) then
    let fact_file = generate_case_file () in
    let fact_cards = [Prof fact_file.who; 
                      Building fact_file.where; 
                      Language fact_file.with_what] in
    let character_lst = assign_characters [] (n+1) in
    let dealt_cards_lst = deal_card fact_cards n in
    let user_hand = List.hd dealt_cards_lst in
    let ai_hand_lst = remove_first_el dealt_cards_lst in
    let user_character = List.hd character_lst in
    let ai_characters_lst = remove_first_el character_lst in
    let ai_lst = init_ai_lst n d ai_hand_lst ai_characters_lst in
    let map = construct_map () in
    let dictionary = generate_dictionary
                      ["Bracy";"Clarkson";"Fan";"Gries";"Halpern";"White"]
                      user_character ai_characters_lst in
    print_map map;
    Printf.printf "The AI bots play the roles of %s.\n" 
                  (string_of_prof_lst ai_characters_lst);
    Printf.printf "You play the role of Prof. %s.\n" user_character;
    print_endline (
      "You start at the position of your last name initial on the map.\n" ^
      "You can move to any white dots [.], \n" ^
      "enter a building through a door [D], \n" ^
      "and teleport to another building if there is a secret passage [s].");
    print_endline "You have the following cards: ";
    Printf.printf "    %s.\n" (string_of_card_lst user_hand);
    print_endline (
      "To play the game, follow the instructions and type into the command line \n" ^
      "when prompted [> ]. You may find taking lots of notes helpful for \n" ^
      "keeping track of cards you have and other players might have. Good luck! \n");
    print_endline "The game begins now!";
    {
      counter = 0;
      game_complete = false;
      map = map;
      user = {character = user_character; hand = user_hand; was_moved = false};
      ais = ai_lst;
      fact_file = fact_file;
      dictionary = dictionary;
      past_guesses = [];
    }
  else
    failwith "This should not happen in init_state in game.ml"

(************************)
(* step and its helpers *)
(************************)

(* [step s] is the updated state after one player's turn. *)
let rec step (s:state) : state =
  if s.game_complete then s else
  match s.counter mod 6 with
  | 0 -> step_helper "Bracy"    s
  | 1 -> step_helper "Clarkson" s
  | 2 -> step_helper "Fan"      s
  | 3 -> step_helper "Gries"    s
  | 4 -> step_helper "Halpern"  s
  | 5 -> step_helper "White"    s
  | _ -> failwith "This should not happen in step in game.ml"
(* [step_helper p s] is the updated state after the player whose character is
 * prof [p] plays his/her turn. *)
and step_helper (p:prof) (s:state) : state =
  match List.assoc p s.dictionary with
  | `AI ->
      let ai = List.find (fun a -> a.character = p) s.ais in
      let news = 
        if ai.is_in_game 
        then 
          begin
            wait_for_user();
            Printf.printf "Prof. %s's turn...\n" p;
            Ai.step ai s
          end
        else s 
      in
      step {news with counter = news.counter + 1}
  | `User ->
      let news = 
        wait_for_user();
        Printf.printf "Prof. %s's turn... which is your turn :)\n" p;
        User.step s in
      step {news with counter = news.counter + 1}
  | `No ->
      step {s with counter = s.counter + 1}

(********)
(* main *)
(********)

(* [main n d] is the main entry point from outside this module to initialize
 * a game with [n] AI bots and a difficulty level of [d] and start playing
 * it. *)
let main n d =
  let s = init_state n (difficulty_of_int d) in
  (*TODO print out user's character and hand*)
  ignore (step s)

