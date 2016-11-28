open Data
open Gmap

(*******************)
(* utility methods *)
(*******************)

(* [init p h d] is the AI data structure that represents an AI playing
 * character [p] on difficulty level [d], with hand [h].
 *)
let init (p:prof) (h:hand) (d:difficulty) : ai =
  let possible = [
    Prof "Bracy";        Prof "Clarkson";      Prof "Fan";
    Prof "Gries";        Prof "Halpern";       Prof "White";
    Building "Baker";    Building "Carpenter"; Building "Duffield";
    Building "Gates";    Building "Klarman";   Building "Olin";
    Building "Phillips"; Building "Rhodes";    Building "Statler";
    Language "Bash";     Language "C";         Language "Java";
    Language "MATLAB";   Language "OCaml";     Language "Python"] in
  {
  character      = p;
  hand           = h;
  difficulty     = d;
  was_moved      = false;
  is_in_game     = true;
  destination    = None;
  known_cards    = h;
  possible_cards = possible;
  }

(* [get_ai p s] is the AI data structure for the AI playing character [p] in
 * state [s].
 * Raises: Not_found if [p] is not represented by an AI in [s].
 *)
let get_ai (p:prof) (s:state) : ai =
  List.find (fun a -> a.character = p) s.ais

(* [get_difficulty ai] is the difficulty level of the AI [ai].
 *)
let get_difficulty (ai:ai) : difficulty =
  ai.difficulty

(* [still_in_game ai] is [true] iff the AI [ai] is still in the game.
 * If that ai is out, s/he can still prove suggestions wrong.
 *)
let still_in_game (ai:ai) : bool =
  ai.is_in_game

(************************************************
 * Methods for interacting with game state
 ************************************************)

(* [update_ai state player guess player2] updates the knowledge of [state] when
 * [player] makes a [guess] that got disproved by [player2]. *)
let update_state_guess state prof1 guess prof2 =
  (*TODO: if the ai has two of the three cards in its hand and the guess is
   * disproved by someone else, then that third card also becomes a known card.*)
  {state with
    past_guesses=(guess, prof1, prof2)::state.past_guesses
  }

(**)
let card_to_string card=
  match card with
  |Prof p -> p
  |Building b -> b
  |Language l -> l

(*[easy_helper_who possible] takes a list of possible cards and returns the
 * first prof that is in the list. This is a helper function for the easy ai.*)
let rec easy_helper_who possible =
  match possible with
  |[]   -> failwith "there are no possible professors"
  |h::t -> if (((int_of_card h) > (-1)) && ((int_of_card h) < 6))
    then card_to_string h else easy_helper_who t

(*[easy_helper_who possible] takes a list of possible cards and returns the
 * first language that is in the list. This is a helper function for the easy ai.
 *)
let rec easy_helper_what possible =
  match possible with
  |[]   -> failwith "there are no possible languages"
  |h::t -> if (((int_of_card h) > 14) && ((int_of_card h) < 21))
    then card_to_string h else easy_helper_what t

(*[easy_helper_where possible] takes a list of possible cards and returns the
 * first language that is in the list. This is a helper function for the easy ai.
 *)
let rec easy_helper_where possible =
  match possible with
  |[]   -> failwith "there are no possible buildings"
  |h::t -> if (((int_of_card h) > 5) && ((int_of_card h) < 15))
    then card_to_string h else easy_helper_where t

(*[make_suggestion building ai] produces a [case_file] that the other players
 * will attempt to disprove. *)
let make_suggestion building ai =
    let loc    = building in
    let perp   = easy_helper_who  ai.possible_cards in
    let weapon = easy_helper_what ai.possible_cards in
    ( ignore (Printf.printf "%s has guessed that the culprit was %s using %s in %s Hall.\n
                    We will now go around and attempt to disprove the guess."
      ai.character, perp, weapon, loc)
    ;
  {who = perp; where = loc; with_what = weapon})

(*[replace_ai_with_new new_ai ai_list] returns an updated list of ais, replacing
the old ai with this new one.*)
let rec replace_ai_with_new new_ai ai_list =
  match ai_list with
  |[]-> failwith "not an ai"
  |h::t-> if h.character=new_ai.character then new_ai::t
          else h::replace_ai_with_new new_ai t

(*[make_accusation state ai] produces a state where the accusation has been made
 * with the case_file that the ai believes is correct. Does not depend on ai
 * difficulty. The ai only makes an accusation
 * when it has narrowed down the possible cards to 3. *)
let make_accusation state ai : state=
    let loc    = easy_helper_where ai.possible_cards in
    let perp   = easy_helper_who   ai.possible_cards in
    let weapon = easy_helper_what  ai.possible_cards in
    let guess  = {who=perp; where=loc; with_what=weapon} in
    Printf.printf "%s has made an accusation that the culprit was %s using
                    %s in %s Hall!\n"
                     ai.character perp weapon loc;
    if guess = state.fact_file
    then
      begin
      print_endline "Uh oh, the AI has won! That accusation was correct.
                          You have lost the game. :(";
      {state with game_complete=true}
      end
    else
      begin
      print_endline "The AI has made the wrong accusation! This AI is now
        out of of the game, though it can still prove your suggestions
        wrong/right, it can no longer win and will not move. ";
      let new_ai      = {ai with is_in_game=false} in
      let new_ai_list = replace_ai_with_new new_ai state.ais in
      {state with ais = new_ai_list}
      end

(*[easy_want_to_accuse] is true when there are only three possible cards left
 * so the ai knows the right answer and thus wants to accuse. *)
let easy_want_to_accuse ai : bool =
  (List.length ai.possible_cards) = 3

(*[updated_state_map state new_map] returns the updated state with new_map.
 * Nothing else in state is changed*)
let updated_state_map state new_map : state =
  let s ={state with map = new_map} in s

(*[character_to_ai] prof ai_list] takes in the prof and a list of ais and returns
 * the ai that corresponds to that professor.
 * requires: the prof must match with a ai that is currently in the game*)
let rec character_to_ai prof ai_list =
  match ai_list with
  |[]    -> failwith "should not occur"
  |h::t  -> if h.character = prof then h else character_to_ai prof t

(*[easy_helper_disprove hand guess] attempts to disprove [guess] with the cards
they have in their [hand]. Returns Some Card that the player uses to disprove
or None if no such card exists. *)
let rec easy_helper_disprove hand guess = match hand with
  |[]   -> None
  |h::t -> if (card_to_string h)=guess.who ||
                    (card_to_string h)=guess.where ||
                    (card_to_string h)=guess.with_what then
      (Some h) else easy_helper_disprove t guess

(* [disprove ai guess] figures out which card to reveal in response
 * to a suggestion [guess].
 * Returns: [Some c] where [c] is a card that [ai] can reveal. Or, if [ai] has
 * none of the cards in [guess], then it will return [None].
 *)
let disprove ai (guess: case_file) =
    easy_helper_disprove ai.hand guess
  (*|Medium -> failwith "unimplemented"
  |Hard   -> (*if one of the cards has already been in a past guess, the ai wants to
   show that one so we give the other players as little information as possible.
    *)
    failwith "unimplemented"*)

(*[help_disprove players state guess] takes in [players], a prof list of the
 * current players, and a state and returns (Some card, Some Prof) that some
 * player disproved the guess with or returns (None, None) if the guess is not
 * disproved. *)
let rec help_disprove players state guess =
    match players with
    |[]   -> (None, None)
    |h::t ->
      begin
        match (List.assoc h state.dictionary) with
        |`AI   ->
          let proof = disprove (character_to_ai h state.ais) guess in
            if   proof = None
            then (Printf.printf "%s was not able to disprove it. " h;
                        help_disprove t state guess)
            else (Printf.printf "%s was able to disprove it. " h; (proof, Some h))
        |`User ->
          let proof = User.disprove state guess in
            if   proof = None
            then (Printf.printf "%s was not able to disprove it. " h;
                        help_disprove t state guess)
            else (Printf.printf "%s was able to disprove it." h;(proof, Some h))
        |`No   -> help_disprove t state guess
      end

(*if c is a prof*)
let rec helper_update_prof poss c=
      match poss with
      |[]   -> [c]
      |h::t -> if int_of_card h > 5 then h::helper_update_prof t c
               else helper_update_prof t c

(*if c is a building*)
let rec helper_update_building poss c=
      match poss with
      |[]   -> [c]
      |h::t -> if (int_of_card h < 6)||(int_of_card h > 14) then
                    h::helper_update_building t c
               else helper_update_building t c

(*if c is a language*)
let rec helper_update_lang poss c=
      match poss with
      |[]   -> [c]
      |h::t -> if int_of_card h <15 then h::helper_update_lang t c
               else helper_update_lang t c

(*[update_possible] returns the updated list of possible cards. If a card was
 * not able to be disproved and the ai knows that it doesn't possess the card,
 * then all other cards of that type (building/prof/language) are removed from
 * the possible cards. If it was not able to be disproved and the ai does
 * possess it, it just returns the old list of possible cards.
 *)
let update_possible ai guess =
  let where = Building guess.where in
  let what = Language guess.with_what in
  let who = Prof guess.who in
  if (List.mem where ai.hand)&&(List.mem what ai.hand)&&(List.mem who ai.hand)
              then ai.possible_cards
              else if (List.mem where ai.hand)&&(List.mem what ai.hand) then
                helper_update_prof ai.possible_cards who
              else if (List.mem where ai.hand)&&(List.mem who ai.hand) then
                helper_update_lang ai.possible_cards what
              else if (List.mem what ai.hand)&&(List.mem who ai.hand) then
                helper_update_building ai.possible_cards where
              else if(List.mem where ai.hand) then
                helper_update_lang (helper_update_prof ai.possible_cards who) what
              else if(List.mem what ai.hand) then
                helper_update_building (helper_update_prof ai.possible_cards who) where
              else if(List.mem who ai.hand) then
                helper_update_building (helper_update_lang ai.possible_cards what) where
              else
                helper_update_building
                      (helper_update_lang
                              (helper_update_prof ai.possible_cards who)
                                            what)
                                                where

(*[update_ai_not_disproved ai guess] updates the [ai] based on the fact that
[guess] was disproved. Returns updated ai.*)
 (*if not disproved, add to past guesses. Get rid of all other cards
              of that type in ai.possible_cards.*)
let update_ai_not_disproved ai guess =
print_endline "No one was able to disprove the AI.";
{ai with possible_cards=update_possible ai guess}

(*[update_ai_disproved ai guess player] takes the card [c] that was returned by
[player] who disproved [guess] and updates [ai].*)
             (*if disproved, add card to known cards. Add this guess to
             past guesses.  *)

let update_ai_disproved ai c guess player =
  print_endline "The AI's guess has been disproved!";
  { ai with known_cards  = c::ai.known_cards }

(* [step a s] peforms a turn for AI player [a]. This involves:
 *   - defining and setting goals by processing knowledge from suggestions and
 *     making deductions about other players' turns.
 *   - moving the AI around the map
 *   - forming and making suggestions/accusations
 * Returns: an updated game state.
 *)
let rec step ai state =
  let moves = roll_two_dice () in
  if ai.is_in_game then
    begin
    if is_in_building state.map ai.character
    then
      begin
      ignore (leave_building state.map ai.character 0);
      step ai state
      end
    else
      begin
      let (i, dest) = List.hd (closest_buildings state.map ai.character) in
      let (in_building, new_map) = move_towards_building state.map
                                            ai.character dest moves in
      if in_building then begin
        let players =
          match ai.character with
                  |"Bracy"   -> ["Clarkson";  "Fan";  "Gries";
                                       "Halpern";  "White"]
                  |"Clarkson"-> [ "Fan";  "Gries";  "Halpern";
                                       "White";  "Bracy"]
                  |"Fan"     -> [ "Gries";  "Halpern";  "White";
                                       "Bracy";  "Clarkson"]
                  |"Gries"   -> [ "Halpern"; "White";  "Bracy";
                                       "Clarkson";  "Fan"]
                  |"Halpern" -> [ "White";  "Bracy"; "Clarkson";
                                       "Fan";  "Gries"]
                  |"White"   -> [ "Bracy"; "Clarkson";  "Fan";
                                       "Gries";  "Halpern"]
                  |_   -> failwith "This should not happen in step" in
          let guess = make_suggestion dest ai in
          let new_state = updated_state_map state new_map in
          let (prof_option, new_ai) =
            match help_disprove players new_state guess with
                    |(None, None)     ->
                                (None  , update_ai_not_disproved ai guess)
                    |(Some c, Some p) ->
                                (Some p, update_ai_disproved     ai c guess p)
                    | _ -> failwith "not a valid option" in
          let new_ai_list = replace_ai_with_new new_ai state.ais in
          if easy_want_to_accuse ai
          then begin
            make_accusation new_state ai
          end
          else begin
            {new_state with
                        ais     = new_ai_list;
                        past_guesses = (guess, ai.character, prof_option)::state.past_guesses
            }

                  (*{
                    counter=state.counter+1;
                    game_complete= game_complete;
                    map=state.map;
                    user=state.user;
                    ais=state.ais;
                    fact_file=state.fact_file;
                    dictionary=state.dictionary;
                  }*)
          end
      end
    else begin
      updated_state_map state new_map
    end
    end
  end
    else state

(********************************************************************)
(* alice's scratch XXD *)

(*TODO*)
(*let accuse (a:ai) (s:state) : state =
  failwith "TODO"

(* TODO decides whether to accuse or not in the middle of an AI's turn
 * AI logic:
 *   - Easy:   simply not accuse and proceed
 *   - Medium: TODO
 *   - Hard:   TODO *)
let accuse_or_not_middle (a:ai) (s:state) : state =
  match a.difficulty with
  | Easy   -> s
  | Medium -> failwith "TODO"
  | Hard   -> failwith "TODO"

(* TODO decides whether to accuse or not at the start of an AI's turn
 * AI logic:
 *   - Easy:   simply not accuse and proceed
 *   - Medium: TODO
 *   - Hard:   TODO *)
let accuse_or_not_start (a:ai) (s:state) : state =
  match a.difficulty with
  | Easy   ->
      begin
      Printf.printf "Prof. %s does not wish to make an accusation right now.\n"
                    a.character;
      s
      end
  | Medium -> failwith "TODO"
  | Hard   -> failwith "TODO"

(*TODO
 * AI logic:
 *   - Easy: simply chooses the first card. *)
let choose_from_two (c1:card) (c2:card) : card option =
  match a.difficulty with
  | Easy   -> Some c1
  | Medium -> failwith "TODO"
  | Hard   -> failwith "TODO"

(*TODO
 * AI logic:
 *   - Easy: simply chooses the first card. *)
let choose_from_three (c1:card) (c2:card) (c3:card) : card option =
  match a.difficulty with
  | Easy   -> Some c1
  | Medium -> failwith "TODO"
  | Hard   -> failwith "TODO"

(*TODO*)
let disprove (a:ai) (guess:case_file) : card option =
  Printf.printf "It is Prof. %s's turn to disprove the suggstion:\n"
                a.character;
  let hand = a.hand in
  let {who; where; with_what} = guess in
  let who_or_not = List.mem (Prof who) hand in
  let where_or_not = List.mem (Building where) hand in
  let with_what_or_not = List.mem (Language with_what) hand in
  match who_or_not, where_or_not, with_what_or_not with
  | true, true, true ->
      Printf.printf "Prof. %s disproved the suggestion.\n" a.character;
      choose_from_three (Prof who) (Building where) (Language with_what)
  | true, true, false ->
      Printf.printf "Prof. %s disproved the suggestion.\n" a.character;
      choose_from_two (Prof who) (Building where)
  | true, false, true ->
      Printf.printf "Prof. %s disproved the suggestion.\n" a.character;
      choose_from_two (Prof who) (Language with_what)
  | true, false, false ->
      Printf.printf "Prof. %s disproved the suggestion.\n" a.character;
      Some (Prof who)
  | false, true, true ->
      Printf.printf "Prof. %s disproved the suggestion.\n" a.character;
      choose_from_two (Building where) (Language with_what)
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

(* [disprove_loop n guess s] is [Some (prof, card)] if [prof] disproved
 * [guess] with [card] and [None] if no one can disprove [guess].
 * It starts with the professor corresponding to integer [n], goes along
 * the loop B->C->F->G->H->W->B until someone is able to disprove [guess]
 * or when the user's character is reached. *)
let rec disprove_loop (ncurrent:int) (n:int) (guess:case_file) (s:state)
                        : ((prof * card) option) =
  let n' = n mod 6 in
  if n' = ncurrent then None else
  match n' with
  | 0 -> disprove_case "Bracy"    ncurrent 0 guess s
  | 1 -> disprove_case "Clarkson" ncurrent 1 guess s
  | 2 -> disprove_case "Fan"      ncurrent 2 guess s
  | 3 -> disprove_case "Gries"    ncurrent 3 guess s
  | 4 -> disprove_case "Halpern"  ncurrent 4 guess s
  | 5 -> disprove_case "White"    ncurrent 5 guess s
  | _ -> failwith "This should not happen in disprove_loop in user.ml"
(* [disprove_case p n guess s] checks if the professor corresponding
 * to integer [n] is represented by any ai and if so, if that ai can disprove
 * [guess], before possibly calling [disprove_loop (n+1) guess s] to move
 * on to check the next professor. *)
and disprove_case (p:prof) (ncurrent:int) (n:int) (guess:case_file) (s:state)
                         : ((prof * card) option) =
  match List.assoc p s.dictionary with
  | `AI ->
      let ai = List.find (fun a -> a.character = p) s.ais in (*TODO use get_ai?*)
      begin
      match disprove ai guess with
      | Some card -> Some (p, card)
      | None -> disprove_loop ncurrent (n+1) guess s
      end
  | `User ->
      begin
      match User.disprove s guess with
      | Some card -> Some (p, card)
      | None -> disprove_loop ncurrent (n+1) guess s
      end
  | `No ->
      disprove_loop ncurrent (n+1) guess s

(*TODO
 * AI logic: very stupid, always makes the same suggestion. *)
let suggest_easy (a:ai) (s:state) : (prof * language) =
  ("Bracy", "Bash")

(*TODO*)
let suggest (a:ai) (s:state) : state =
  Printf.printf "Prof. %s is making a suggestion about the current building.\n"
                a.character;
  let where_option = get_current_building s.map a.character in (* Gmap *)
  match where_option with
  | Some where ->
    let (who, with_what) =
      begin
      match a.difficulty with
      | Easy   -> suggest_easy a s
      | Medium -> failwith "TODO"
      | Hard   -> failwith "TODO"
      end
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
    let ncurrent = int_of_card (Prof a.character) in
    begin
    match disprove_loop ncurrent (ncurrent+1) guess s with
    | Some (p, c) ->
        Printf.printf "Prof. %s disproved Prof. %s's suggestion.\n"
                      p a.character;
        let newais = List.map (fun a' ->
          if a' <> a then a'
          else {a with known_cards = c::a.known_cards})
          s.ais in
        let news'' =
          {news' with ais = newais;
                      past_guesses = (*TODO possibly have a helper.ml?*)
                      (guess, a.character, Some p)::news'.past_guesses;} in
        accuse_or_not_middle news''
    | None ->
        Printf.printf "No one can disprove Prof. %s's suggestion.\n"
                      a.character;
        let news'' =
          {news' with past_guesses =
                      (guess, a.character, None)::news'.past_guesses} in
        accuse_or_not_middle news''
    end
  | None -> failwith "This should not happen in suggest in ai.ml"

(*TODO
 * Requires: n > 0.
 * AI logic: finds the closest building and tries to enter that one
 *           if does not already have a destination. *)
let move_easy (a:ai) (n:int) (s:state) : state =
  if is_in_building s.map a.character (* Gmap *)
  then
    suggest a s
  else
    let coord =
      begin
      match a.destination with
      | Some c -> c
      | None ->
          List.hd (closest_buildings s.map a.character) (* Gmap *)
      end
    in
    let (in_building, new_map) =
      move_towards_building s.map a.character coord n in (* Gmap *)
    if in_building
    then suggest a {s with map = new_map} (*TODO print?*)
    else {s with map = new_map} (*TODO print?*)

(* [move a n s] allows the AI [a]'s character to move n steps,
 * or fewer if the character gets into a building
 * before using up all the steps. *)
let move (a:ai) (n:int) (s:state) : state =
  if n < 0 then
    failwith "This should not happen in user_move"
  else if n = 0 then
    begin
    Printf.printf "Prof. %s cannot move anymore.\n" a.character;
    if is_in_building s.map a.character (* Gmap *)
    then suggest a s
    else s
    end
  else
    match a.difficulty with
    | Easy   -> move_easy a n s
    | Medium -> failwith "TODO"
    | Hard   -> failwith "TODO"

(*TODO*)
let in_building_involuntarily (a:ai) (b:building) (s:state) : state =
  let secret = has_secret_passage s.map b in
  let blocked = is_building_blocked s.map b in
  let news =
    begin
    match secret, blocked with
    | true,  true  ->
        suggest_or_secret a b s (*TODO*)
    | true,  false ->
        secret_or_roll_or_suggest a b s (*TODO*)
    | false, true  ->
        suggest a s
    | false, false ->
        suggest_or_roll a s (*TODO*)
    end
  in assign_was_moved news a.character false

(*TODO*)
let in_building_voluntarily (a:ai) (b:building) (s:state) : state =
  let secret = has_secret_passage s.map b in (* Gmap *)
  let blocked = is_building_blocked s.map b in (* Gmap *)
  match secret, blocked with
  | true,  true  ->
      print_endline "AI has to use the secret passage.";
      use_secret a s (*TODO*)
  | true,  false ->
      print_endline "There is a secret passage available.";
      secret_or_roll a b s (*TODO*)
  | false, true  ->
      print_endline "AI has to wait until your next turn.";
      s
  | false, false ->
      move a (roll_two_dice ()) s

(* [step a s] peforms a turn for AI player [a]. This involves:
 *   - defining and setting goals by processing knowledge from suggestions and
 *     making deductions about other players' turns.
 *   - moving the AI around the map
 *   - forming and making suggestions/accusations
 * Returns: an updated game state.
 *)
let rec step (a:ai) (s:state) : state =
  let s1 = accuse_or_not_start a s in
  if s1.game_complete then s1 else
  match get_current_building s1.map a.character with (* Gmap *)
  | Some b ->
      if a.was_moved
      then in_building_involuntarily a b s1
      else in_building_voluntarily a b s1
  | None ->
      move a (roll_two_dice ()) s1 (* Gmap *)
    *)
