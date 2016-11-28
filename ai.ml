open Data
open Gmap

(***********************************************
 * Utility methods
 ***********************************************)

(* [init p d hand] creates and returns an AI data structure that represents an
 * AI playing character [p] on difficulty [d], with starting hand [hand].
 * Returns: the initialized setup for an AI.
 *)
let init p d hand =
  let possible = [
    Prof "Bracy";        Prof "Clarkson";      Prof "Fan";
    Prof "Gries";        Prof "Halpern";       Prof "White";
    Building "Baker";    Building "Carpenter"; Building "Duffield";
    Building "Gates";    Building "Klarman";   Building "Olin";
    Building "Phillips"; Building "Rhodes";    Building "Statler";
    Language "Bash";     Language "C";         Language "Java";
    Language "MATLAB";   Language "OCaml";     Language "Python"] in
    {
      character   = p;
      hand        = hand;
      was_moved   = false;
      is_in_game  = true;
      difficulty  = d;
      destination = None;
      known_cards = hand;
      possible_cards = possible;
    }

let rec help_get_ai ais p=
  match ais with
  |[]-> failwith "character not in play"
  |h::t-> if (h.character = p) then h else (help_get_ai t p)

(* [get_ai state p]
 * Returns: the AI data structure for the AI playing character [p]
 *)
let get_ai state p =
  help_get_ai state.ais p

(* [get_difficulty ai]
 * Returns: the difficulty of the AI [ai].
 *)
let get_difficulty ai =
  ai.difficulty

(* [still_in_game ai]
 * Returns: [true] iff the AI [ai] is still in the game. (If you're out you
 * can still prove suggestions wrong)
 *)
let still_in_game ai =
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
    (Printf.printf "%s has guessed that the culprit was %s using %s in %s Hall.\n
                    We will now go around and attempt to disprove the guess."
      ai.character, perp, weapon, loc
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
let make_accusation state ai =
    let loc    = easy_helper_where ai.possible_cards in
    let perp   = easy_helper_who   ai.possible_cards in
    let weapon = easy_helper_what  ai.possible_cards in
    let guess  = {who=perp; where=loc; with_what=weapon} in
    (Printf.printf "%s has made an accusation that the culprit was %s using
                    %s in %s Hall!\n"
                     ai.character, perp, weapon, loc;
    (if guess = state.fact_file
    then
      (print_endline "Uh oh, the AI has won! That accusation was correct.
                          You have lost the game. :(";
       state = {state with game_complete=true})
    else
      (print_endline "The AI has made the wrong accusation! This AI is now
        out of of the game, though it can still prove your suggestions
        wrong/right, it can no longer win and will not move. ";
        let new_ai      = {ai with is_in_game=false} in
        let new_ai_list = replace_ai_with_new new_ai state.ais in
          state = {state with ais = new_ai_list})
    )
)
(*[easy_want_to_accuse] is true when there are only three possible cards left
 * so the ai knows the right answer and thus wants to accuse. *)
let easy_want_to_accuse ai =
  (List.length ai.possible_cards) = 3

(*[updated_state_map state new_map] returns the updated state with new_map.
 * Nothing else in state is changed*)
let updated_state_map state new_map =
  {state with map = new_map}

(*[character_to_ai] prof ai_list] takes in the prof and a list of ais and returns
 * the ai that corresponds to that professor.
 * requires: the prof must match with a ai that is currently in the game*)
let rec character_to_ai prof ai_list =
  match ai_list with
  |[]    -> failwith "should not occur"
  |h::t  -> if h.character = prof then h else character_to_ai prof t

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
        |`Ai   ->
          let proof = ai_disprove (character_to_ai h state.ais) guess in
            if   proof = None
            then help_disprove t state guess
            else (proof, Some h)
        |`User ->
          let proof = User.user_disprove state guess in
            if   proof = None
            then help_disprove t state guess
            else (proof, Some h)
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
  let where = guess.where in
  let what = guess.with_what in
  let who = guess.who in
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

(* [step state p] peforms a turn for player [p] (an AI player). This involves:
 *   - defining and setting goals by processing knowledge from suggestions and
 *     making deductions about other players' turns.
 *   - moving the AI around the map
 *   - forming and making suggestions/accusations
 * Returns: an updated game state.
 *)
let ai_step state ai =
  let moves = roll_two_dice () in
    if ai.is_in_game then
      begin
        if is_in_building state.map ai.character
        then
          (leave_building state.map 0;
           step state ai)
        else
          begin
            let dest = List.hd (closest_buildings state.buildling ai.character) in
            let (in_building, new_map) = move_towards_building state.map
                                         ai.character moves dest in
              if in_building then
                let players = match ai.character with
                  |Prof "Bracy"   -> [Prof "Clarkson"; Prof "Fan"; Prof "Gries";
                                      Prof "Halpern"; Prof "White"]
                  |Prof "Clarkson"-> [Prof "Fan"; Prof "Gries"; Prof "Halpern";
                                      Prof "White"; Prof "Bracy"]
                  |Prof "Fan"     -> [Prof "Gries"; Prof "Halpern"; Prof "White";
                                      Prof "Bracy"; Prof "Clarkson"]
                  |Prof "Gries"   -> [Prof "Halpern";Prof "White"; Prof "Bracy";
                                      Prof "Clarkson"; Prof "Fan"]
                  |Prof "Halpern" -> [Prof "White"; Prof "Bracy";Prof "Clarkson";
                                      Prof "Fan"; Prof "Gries"]
                  |Prof "White"   -> [Prof "Bracy";Prof "Clarkson"; Prof "Fan";
                                      Prof "Gries"; Prof "Halpern"] in
                  let guess  = make_suggestion dest ai in
                  let state  = updated_state_map new_map in
                  let (prof_option, new_ai) =
                    match help_disprove players state guess with
                    |(None, None)     ->
                                (None  , update_ai_not_disproved ai guess)
                    |(Some c, Some p) ->
                                (Some p, update_ai_disproved     ai c guess p)
                    | _ -> failwith "not a valid option" in
                  let new_ai_list = replace_ai_with_new new_ai state.ais in
                    (if easy_want_to_accuse ai.possible_cards
                    then
                      make_accusation state ai
                    else
                      { state with
                        map = new_map;
                        counter = state.counter+1;
                        ais     = new_ai_list;
                        past_guesses = (guess, ai.character, prof_option)::state.past_guesses
                      })
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
      else
      begin
        updated_state_map new_map
      end
   (* else state*)

(*[easy_helper_disprove hand guess] attempts to disprove [guess] with the cards
they have in their [hand]. Returns Some Card that the player uses to disprove
or None if no such card exists. *)
let rec easy_helper_disprove hand guess = match hand with
  |[]   -> None
  |h::t -> if h=guess.who || h=guess.where || h=guess.with_what then
      (Some h) else easy_helper_disprove t guess

(* [disprove ai guess] figures out which card to reveal in response
 * to a suggestion [guess].
 * Returns: [Some c] where [c] is a card that [ai] can reveal. Or, if [ai] has
 * none of the cards in [guess], then it will return [None].
 *)
let ai_disprove ai (guess: case_file) =
    easy_helper_reveal ai.hand guess
  (*|Medium -> failwith "unimplemented"
  |Hard   -> (*if one of the cards has already been in a past guess, the ai wants to
   show that one so we give the other players as little information as possible.
    *)
    failwith "unimplemented"*)
