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

(*[deal_card] gives a list of hands given the number of AI bots and the cards
 * containing the fact file.
 * requires: [fact_card] is a list of cards indicating the fact file.
 *           [n] is an integer between 2 and 5 inclusive.*)
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

(*[init_ai_lst] is a list of AI bots given the number of AIs in the game,
 * the difficulty level, list of hands that AI bots have, and list of
 * characters of each AI bots.
 * requires: [n] is an integer between 2 and 5 inclusive.
 *           [hand_lst] is a list of hands
 *           [character_lst] is a string list.*)
let init_ai_lst n d hand_lst ai_character_lst character_lst =
  let ai_lst = ref [] in
  while (List.length !ai_lst < n) do (
    let nth = List.length !ai_lst in
    let character = List.nth ai_character_lst nth in
    let hand = List.nth hand_lst nth in
    let ai = Ai.init character hand d character_lst in
    ai_lst := (!ai_lst) @ [ai]
  ) done;
  !ai_lst

(*[generate_dictionary] is the dictionary generated given by the list of
 * professors, user character, and list of ai's characters.
 * requires: [prof_lst],[ai_lst] are string lists
 *           [user_char] is string indicating character*)
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
  Random.self_init (); (*Random seed*)
  if (n >= 2) && (n <= 5) then
    let fact_file  = generate_case_file () in
    let fact_cards = [Prof fact_file.who;
                      Building fact_file.where;
                      Language fact_file.with_what] in
    let character_lst   = assign_characters [] (n+1) in
    let dealt_cards_lst = deal_card fact_cards n in
    let user_hand   = List.hd dealt_cards_lst in
    let ai_hand_lst = List.tl dealt_cards_lst in
    let user_character    = List.hd character_lst in
    let ai_characters_lst = List.tl character_lst in
    let ai_lst = init_ai_lst n d ai_hand_lst ai_characters_lst character_lst in
    let map    = construct_map () in
    let dictionary = generate_dictionary
                      ["Bracy";"Clarkson";"Fan";"Gries";"Halpern";"White"]
                      user_character ai_characters_lst in 
    print 
      "**********************************************************************"
       true;

    print_results "The AI bots play the roles of: " true;
    print_results ((string_of_prof_lst ai_characters_lst)) true;
    print_info " " true;
    print_important
      ("You play the role of Prof. "^user_character^".") true;
    wait_for_user();

    ANSITerminal.(
      print_info "Your location is tracked on the map by your last name " 
                  false;
      print_string [on_black; Bold; cyan] "I";
      print_string [on_black; yellow] "nitial.        ";
      print_endline "";
      print_info "You can move on any spot marked by " false;
      print_string [on_black; Bold; white] ".";
      print_string [on_black; yellow]  ",                                 ";
      print_endline "";
      print_info "Enter a building through a door " false;
      print_string [on_black; Bold; green] "D";
      print_string [on_black; yellow] ", and                                ";
      print_endline "";
      print_info "Use a secret passage " false;
      print_string [on_black; Bold; green] "s";
      print_string [on_black; yellow] 
                    " to get to the building diagonally across the   ";
      print_endline "";
      print_info 
      "map. Note: the map is on a coordinate system, with (0,0) at the top   " 
      true;
      print_info "left corner." true;
      print_info "" true;
    );
    print_map map;
    wait_for_user();

    ANSITerminal.(
      print_info "You have the following cards (take notes please!): " true;
      print_card_list user_hand;
      print_info "" true;
      print_info 
        "To play the game, follow the instructions and type into the command" 
        true;
      print_info "line when prompted by " false;
      print_string [on_black] ">";
      print_info ". You may find having a sheet of paper and a   " false;
      print_endline "";
      print_info "writing utensil handy when playing the game. Good luck!" 
                  true;
      print_important "The game begins now!" true;
    );
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

(* [check_ai_in_game] is false if all ai bots have lost and is true
 * if there is at least one bot still in game.
 * requires: [ai_lst] is a list of ai bots*)
let rec check_ai_in_game (ai_lst: ai list) : bool =
   match ai_lst with
   | [] -> false
   | h::t -> if h.is_in_game then
                true
             else
                false || (check_ai_in_game t)

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
            print_important
              ("----------------:: Prof. "^p^"'s turn ::----------------")
              true;
            Ai.step ai s
          end
        else s
      in
      if check_ai_in_game s.ais then
        step {news with counter = news.counter + 1}
      else
        begin
          print_important "Wow! All the AI bots have lost!" true;
          print_win ();
          step {news with counter = news.counter + 1; game_complete = true}
        end
  | `User ->
      let news =
        wait_for_user();
        print_important
          ("----------------:: Prof. "^p^"'s (You!) turn ::---------------")
          true;
        print_map s.map;
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
  ignore (step s)

