open Data
open Gmap
open Logic

(*******************)
(* utility methods *)
(*******************)

(*[known_not_possible ai] updates ai.possible_cards to not include the cards
from ai.known_cards*)
let known_not_possible ai =
  let rec helper known possible =
        match possible with
        |[]   -> []
        |h::t -> if List.mem h known then (helper known t) else h::(helper known t)
  in
  let new_poss = helper ai.known_cards ai.possible_cards in
  {ai with possible_cards=new_poss}

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
  let a1= {
    character      = p;
    hand           = h;
    difficulty     = d;
    was_moved      = false;
    is_in_game     = true;
    destination    = None;
    known_cards    = h;
    possible_cards = possible;
  } in
  known_not_possible a1

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
 * Helper Functions
 ************************************************)

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

(*[replace_ai_with_new new_ai ai_list] returns an updated list of ais, replacing
the old ai with this new one.*)
let rec replace_ai_with_new new_ai ai_list =
  match ai_list with
  |[]-> failwith "not an ai"
  |h::t-> if h.character=new_ai.character then new_ai::t
          else h::replace_ai_with_new new_ai t

(*[easy_want_to_accuse] is true when there are only three possible cards left
 * so the ai knows the right answer and thus wants to accuse. *)
let easy_want_to_accuse ai : bool =
  (List.length ai.possible_cards) = 3

(*[character_to_ai] prof ai_list] takes in the prof and a list of ais and returns
 * the ai that corresponds to that professor.
 * requires: the prof must match with a ai that is currently in the game*)
let rec character_to_ai prof ai_list =
  match ai_list with
  |[]    -> failwith "should not occur"
  |h::t  -> if h.character = prof then h else character_to_ai prof t

(*[updated_state_map state new_map] returns the updated state with new_map.
 * Nothing else in state is changed*)
let updated_state_map state new_map : state =
  let s ={state with map = new_map} in s

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

(*[update_possible_not_disproved] returns the updated list of possible cards. If a card was
 * not able to be disproved and the ai knows that it doesn't possess the card,
 * then all other cards of that type (building/prof/language) are removed from
 * the possible cards. If it was not able to be disproved and the ai does
 * possess it, it just returns the old list of possible cards.
 *)
let update_possible_not_disproved ai guess =
  print_endline "No one was able to disprove the guess: \n";
  print_case_file guess;
  let where = Building guess.where in
  let what  = Language guess.with_what in
  let who   = Prof guess.who in
  let have_where = List.mem where ai.hand in
  let have_what  = List.mem what  ai.hand in
  let have_who   = List.mem who   ai.hand in
    if have_where && have_what && have_who
      then ai.possible_cards
    else if have_where && have_what then
      helper_update_prof ai.possible_cards who
    else if have_where && have_who then
      helper_update_lang ai.possible_cards what
    else if have_what && have_who then
      helper_update_building ai.possible_cards where
    else if have_where then
      helper_update_lang (helper_update_prof ai.possible_cards who) what
    else if have_what then
      helper_update_building (helper_update_prof ai.possible_cards who) where
    else if have_who then
      helper_update_building (helper_update_lang ai.possible_cards what) where
    else
      helper_update_building
        (helper_update_lang
          (helper_update_prof ai.possible_cards who)
        what)
      where

(************************************************
 * Methods for interacting with game state
 ************************************************)
(*
(* [update_ai state player guess player2] updates the knowledge of [state] when
 * [player] makes a [guess] that got disproved by [player2]. *)
let update_state_guess state prof1 guess prof2 =
  (*TODO: if the ai has two of the three cards in its hand and the guess is
   * disproved by someone else, then that third card also becomes a known card.*)
  {state with
    past_guesses=(guess, prof1, prof2)::state.past_guesses
  }

(*[make_suggestion building ai] produces a [case_file] that the other players
 * will attempt to disprove. *)
let make_suggestion building ai =
    let loc    = building in
    let perp   = easy_helper_who  ai.possible_cards in
    let weapon = easy_helper_what ai.possible_cards in
    (Printf.printf "%s has guessed that the culprit was %s using %s in
      %s Hall.\n We will now go around and attempt to disprove the guess."
      ai.character, perp, weapon, loc;
      {who = perp; where = loc; with_what = weapon})

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
      %s in %s Hall!\n" ai.character perp weapon loc;
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
          let proof = ai_disprove (character_to_ai h state.ais) guess in
            if   proof = None
            then (Printf.printf "Prof. %s was not able to disprove it.\n" h;
                   help_disprove t state guess)
            else (Printf.printf "Prof. %s was able to disprove it.\n" h;
                   (proof, Some h))
        |`User ->
          let proof = user_disprove state guess in
            if   proof = None
            then (Printf.printf "Prof. %s was not able to disprove it.\n" h;
                   help_disprove t state guess)
            else (Printf.printf "Prof. %s was able to disprove it.\n" h;
                   (proof, Some h))
        |`No   -> help_disprove t state guess
      end

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
  { ai with known_cards  = c::ai.known_cards }

(* [step a s] peforms a turn for AI player [a]. This involves:
 *   - defining and setting goals by processing knowledge from suggestions and
 *     making deductions about other players' turns.
 *   - moving the AI around the map
 *   - forming and making suggestions/accusations
 * Returns: an updated game state.
 *)
let rec step ai state =
  if not ai.is_in_game then
  (* out of the game, can't do anything. *)
    state
  else
    (* still in game, move around and make suggestions *)
    let moves = roll_two_dice () in
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
            if in_building
            then
              begin
                let players = match ai.character with
                  |"Bracy"   -> ["Clarkson";  "Fan";  "Gries"; "Halpern";  "White"]
                  |"Clarkson"-> [ "Fan";  "Gries";  "Halpern"; "White";  "Bracy"]
                  |"Fan"     -> [ "Gries";  "Halpern";  "White"; "Bracy";  "Clarkson"]
                  |"Gries"   -> [ "Halpern"; "White";  "Bracy"; "Clarkson";  "Fan"]
                  |"Halpern" -> [ "White";  "Bracy"; "Clarkson"; "Fan";  "Gries"]
                  |"White"   -> [ "Bracy"; "Clarkson";  "Fan"; "Gries";  "Halpern"]
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
            { new_state with
                ais     = new_ai_list;
                past_guesses = (guess, ai.character, prof_option)::state.past_guesses
            }
          end
      end
    else begin
      updated_state_map state new_map
    end
  end

*)
(********************************************************************)
(* alice's scratch XXD *)



(*[replace_ai_with_new new_ai ai_list] returns an updated list of ais, replacing
the old ai with this new one.*)
let rec replace_ai_with_new new_ai ai_list =
  match ai_list with
  |[]-> failwith "not an ai"
  |h::t-> if h.character=new_ai.character then new_ai::t
          else h::replace_ai_with_new new_ai t

(*[accuse ai state] produces a state where the accusation has been made
 * with the case_file that the ai believes is correct. Does not depend on ai
 * difficulty. The ai only makes an accusation
 * when it has narrowed down the possible cards to 3. *)
let accuse ai state : state=
    let loc    = easy_helper_where ai.possible_cards in
    let perp   = easy_helper_who   ai.possible_cards in
    let weapon = easy_helper_what  ai.possible_cards in
    let guess  = {who=perp; where=loc; with_what=weapon} in
    Printf.printf "%s has made an accusation that the culprit was %s using
      %s in %s Hall!\n" ai.character perp weapon loc;
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
      match ai_disprove ai guess with
      | Some card -> Some (p, card)
      | None -> disprove_loop ncurrent (n+1) guess s
      end
  | `User ->
      begin
      match user_disprove s guess with
      | Some card -> Some (p, card)
      | None -> disprove_loop ncurrent (n+1) guess s
      end
  | `No ->
      disprove_loop ncurrent (n+1) guess s
(*
(*[suggest_easy building ai] produces a [case_file] that the other players
 * will attempt to disprove. *)
let suggest ai state =
    let loc    = building in
    let perp   = easy_helper_who  ai.possible_cards in
    let weapon = easy_helper_what ai.possible_cards in
    Printf.printf "%s has guessed that the culprit was %s using %s in
      %s Hall.\n We will now go around and attempt to disprove the guess."
      ai.character perp weapon loc;
      {who = perp; where = loc; with_what = weapon})
*)

(* AI logic: random. *)
let suggest_easy (a:ai) (s:state) : (prof * language) =
  (prof_of_int (Random.int 6), lang_of_int (Random.int 6))

(* AI logic: only guesses possible cards, not the ones that it knows exists.*)
let suggest_medium (ai:ai) (s:state) : (prof * language) =
  (easy_helper_who ai.possible_cards, easy_helper_what ai.possible_cards)

(*called move towards building while still in building*)
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
      | Medium -> suggest_medium a s
      | Hard   -> failwith "TODO"
      end in
    let guess =
      {who = who;
       where = where;
       with_what = with_what} in
    print_endline "The suggestion is: ";
    print_case_file guess;
    let (moved_or_not, map) =
      if get_current_building s.map who <> (Some where) (* Gmap *)
      then (true, (teleport_professor s.map who where)) (* Gmap *)
      else (false, s.map) in
    let news = {s with map = map} in
    let news' = assign_was_moved news who moved_or_not in (* Gmap *)
    let ncurrent = int_of_card (Prof a.character) in
    begin
    match disprove_loop ncurrent (ncurrent+1) guess s with
    | Some (p, c) ->
        let newais = List.map (fun a' ->
          if a' <> a then a'
          else known_not_possible {a with known_cards = c>::a.known_cards})
          s.ais in
        let news'' =
          {news' with ais = newais;
                      past_guesses = (*TODO possibly have a helper.ml?*)
                      (guess, a.character, Some p)>::news'.past_guesses;} in
        accuse_or_not_middle a news''
    | None ->
        Printf.printf "No one can disprove Prof. %s's suggestion.\n"
                      a.character;
        let news'' =
          {news' with past_guesses =
                      (guess, a.character, None)>::news'.past_guesses} in
        accuse_or_not_middle a news''
    end
  | None -> failwith "This should not happen in suggest in ai.ml"


(*TODO
 * Requires: n > 0.
 * AI logic: finds the closest building and tries to enter that one
 *           if does not already have a destination. *)
let move_easy (a:ai) (n:int) (s:state) : state =
  let (_, b) = List.hd (closest_buildings s.map a.character) in
  let (in_building, new_map) =
    move_towards_building s.map a.character b n in (* Gmap *)
  if in_building
  then suggest a {s with map = new_map}
  else {s with map = new_map}

(* Requires: n > 0.
 * AI logic: wants to go into a building in the possible list *)
let move_medium a n s : state = failwith "unim"

(* Requires: n > 0.
 * AI logic: wants to go into a building in the possible list that the
ai is closest to*)

let move_hard a n s : state = failwith "unim"
(* [move a n s] allows the AI [a]'s character to move n steps,
 * or fewer if the character gets into a building
 * before using up all the steps. *)
let move (a:ai) (n:int) (s:state) : state =
  if n < 0 then
    failwith "This should not happen in ai_move"
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

(* [get_exit b s] is the id of an exit to building [b] selected by the user. *)
let rec get_exit (ai:ai)  (b:building) (s:state) : int =
  let exits = List.assoc b s.map.exits in
  match List.length exits with
  | 1 -> 1
  | 2 -> get_choice_two () (* Logic *)
  | 4 -> get_choice_four () (* Logic *)
  | _ -> failwith "This should not happen in get_exit in User given map.json"

(*
(* [leave_and_move b s] is the updated state after the user moves out of
 * building [b].
 * Requires: [s.user] is currently in building [b]. *)
let leave_and_move (ai:ai)  (b:building) (s:state) : state =
  let map = leave_building s.map s.user.character (get_exit b s) in
  move (roll_two_dice ()) {s with map = map}
*)

(* [use_secret a s] is the updated state after ai [a] uses the secret
 * passage in the current building.
 * Requires: [a] is currently in a building where there is a secret
 *           passage. *)
let use_secret (a:ai) (s:state) : state =
  let map = use_secret_passage s.map a.character in (* Gmap *)
  suggest a {s with map = map}


(* [suggest_or_secret a b s] allows ai [a] to choose between making a
 * suggestion and using the secret passage to enter building [b], and returns
 * the updated state. *)
let suggest_or_secret (a:ai) (b:building) (s:state) : state =
  match a.difficulty with
  | Easy   -> suggest a s
  | Medium ->
      begin
      match Random.int 1 with
      | 0 -> suggest a s
      | 1 -> use_secret a s
      | _ -> failwith "will never be called"
      end
  | Hard   -> failwith "TODO"

(* [secret_or_roll a b s] allows ai [a] to choose between using the secret
 * passage to enter building [b] and rolling the dice to move out, and returns
 * the updated state. *)
let secret_or_roll (a:ai) (b:building) (s:state) : state =
  match a.difficulty with
  | Easy   -> use_secret a s
  | Medium -> failwith "TODO depends on which is easier to get to destination"
  | Hard   -> failwith "TODO" (* move a (roll_two_dice ()) s *)

(* [suggest_or_roll a s] allows ai [a] to choose between making a suggestion
 * and rolling the dice to move out, and returns the updated state. *)
let suggest_or_roll (a:ai) (s:state) : state =
  match a.difficulty with
  | Easy   -> suggest a s
  | Medium -> failwith "TODO depends on which is easier to get to destination"
  | Hard   -> failwith "TODO" (* move a (roll_two_dice ()) s *)

(* [secret_or_roll_or_suggest a s] allows ai [a] to choose to use the secret
 * passage, or roll the dice to move out, or make a suggestion, and returns
 * the updated state. *)
let secret_or_roll_or_suggest (a:ai) (b:building) (s:state) : state =
  match a.difficulty with
  | Easy   -> suggest a s
  | Medium -> failwith "TODO depends on which is easier to get to destination"
  | Hard   -> failwith "TODO" (* move a (roll_two_dice ()) s *)

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
  if not (still_in_game a) then s1 else
  match get_current_building s1.map a.character with (* Gmap *)
  | Some b ->
      if a.was_moved
      then in_building_involuntarily a b s1
      else in_building_voluntarily a b s1
  | None ->
      move a (roll_two_dice ()) s1 (* Gmap *)

