open Data
open Gmap
open User

(* [t] is the data type representing the Ai's state and goals *)
type t = ai

(***********************************************
 * Utility methods
 ***********************************************)

(* [init p d hand] creates and returns an AI data structure that represents an
 * AI playing character [p] on difficulty [d], with starting hand [hand].
 * Returns: the initialized setup for an AI.
 *)
let init p d hand = {
  character =p;
  hand=hand;
  was_moved=false;
  is_in_game=true;
  difficulty=d;
  destination=None;
  known_cards=hand;
  possible_cards=[];
  past_guesses=[]
  (*(case_file * prof * (prof option)) list;*)
}

let rec help_get_ai ais p=
  match ais with
  |[]-> failwith "character not in play"
  |h::t-> if (h.character = p) then h else (get_ai t p)

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

(* [update_ai ai player guess player2] updates the knowledge of [ai] when
 * [player] makes a [guess] that got disproved by [player2]. *)
let update_ai ai player guess player2 =
  (*TODO: if the ai has two of the three cards in its hand and the guess is
   * disproved by someone else, then that third card also becomes a known card.*)
{
  character =p;
  hand=ai.hand;
  was_moved=ai.was_moved;
  is_in_game=ai.is_in_game;
  difficulty=ai.difficulty;
  destination=ai.destination;
  known_cards=ai.known_cards;
  possible_cards=ai.possible_cards;
  past_guesses=ai.past_guesses^(guess, player, player2)
}

(*[easy_helper_who possible] takes a list of possible cards and returns the
 * first prof that is in the list. This is a helper function for the easy ai.*)
let rec easy_helper_who possible =
              match possible with
              |[]   -> failwith "there are no possible professors"
              |h::t -> if (((Data.int_of_card h)>-1)||((Data.int_of_card h)<6))
               then h else easy_helper_who t

(*[easy_helper_who possible] takes a list of possible cards and returns the
 * first language that is in the list. This is a helper function for the easy ai.
 *)
let rec easy_helper_what possible =
              match possible with
              |[]   -> failwith "there are no possible languages"
              |h::t -> if (((Data.int_of_card h)>14)||((Data.int_of_card h)<21))
              then h else easy_helper_what t

(*[easy_helper_where possible] takes a list of possible cards and returns the
 * first language that is in the list. This is a helper function for the easy ai.
 *)
let rec easy_helper_where possible=
                    match possible with
              |[]   -> failwith "there are no possible buildings"
              |h::t -> if (((Data.int_of_card h)>5)||((Data.int_of_card h)<15))
              then h else easy_helper_where t

(*[make_suggestion building ai] produces a [case_file] that the other players
 * will attempt to disprove. *)
let make_suggestion building ai =
  match ai.difficulty with
  |Easy   ->
    let loc = building in
    let perp = easy_helper_who ai.possible_cards in
    let weapon= easy_helper_what ai.possible_cards in
    {who=perp; where=loc; with_what=weapon}
  |Medium -> failwith "unimplemented"
  |Hard   ->failwith "unimplemented"

(*[make_accusation state ai] produces a [case_file] that the ai believes is
 * correct. Does not depend on ai difficulty. The ai only makes an accusation
 * when it has narrowed down the possible cards to 3. *)
let make_accusation state ai =
    let loc = easy_helper_where ai.possible_cards in
    let perp = easy_helper_who ai.possible_cards in
    let weapon= easy_helper_what ai.possible_cards in
    {who=perp; where=loc; with_what=weapon}


(*[easy_want_to_accuse] is true when there are only three possible cards left
 * so the ai knows the right answer and thus wants to accuse. *)
let easy_want_to_accuse possible_cards ai =
    if (List.length possible_cards)=3 then true else false

(*[updated_state_map state new_map] returns the updated state with new_map.
 * Nothing else in state is changed*)
let updated_state_map state new_map= {
  counter=state.counter;
  game_complete= game_complete;
  map=new_map;
  user=state.user;
  ais=state.ais;
  fact_file=state.fact_file;
  dictionary=state.dictionary;
}

(*[help_disprove state guess] returns Some card that some player disproved the
 * guess with or returns None if the guess is not disproved. *)
let rec help_disprove players guess=
    match players with
    |[]   -> None
    |h::t -> User.disprove guess h else help_disprove t guess

(*[update_ai_not_disproved ai guess] updates the [ai] based on the fact that
[guess] was disproved. Returns updates ai.*)
 (*if not disproved, add to past guesses. Get rid of all other cards
              of that type in ai.possible_cards. check if we can make an
            accustaion. If so, we make it, if not, ai ends its turn and the game
          continues*)
let update_ai_not_disproved ai guess=
let new_possible =
      (**)
{
  character =ai.character;
  hand=ai.hand;
  was_moved=ai.was_moved;
  is_in_game=ai.is_in_game;
  difficulty=ai.difficulty;
  destination=ai.destination;
  known_cards=ai.known_cards;
  possible_cards=ai.possible_cards;
  past_guesses=ai.past_guesses^(guess, ai.character, None)
  (*(case_file * prof * (prof option)) list;*)
}


(*[update_ai_disproved ai guess player] takes the card [c] that was returned by
[player] who disproved [guess] and updates [ai].*)
             (*if disproved, add card to known cards. Add this guess to
             past guesses. Check if we want to make an accusation
              - if so , make it, else, game continues and
           this turn ends *)
let update_ai_disproved ai c guess player=(*TODO*)


(* [step state p] peforms a turn for player [p] (an AI player). This involves:
 *   - defining and setting goals by processing knowledge from suggestions and
 *     making deductions about other players' turns.
 *   - moving the AI around the map
 *   - forming and making suggestions/accusations
 * Returns: an updated game state.
 *)
let step state ai =
  let moves = User.roll_two_dice () in
  if ai.is_in_game then (
    if Gmap.is_in_building state.map ai.character then
      (Gmap.leave_building state.map 0);
      (step state ai) else

  match ai.difficulty with
  |Easy-> let dest = (Gmap.closest_buildings state.buildling ai.character)[0] in
            (
            match (Gmap.move_towards_building state.map ai.character moves dest) with
            |x, y -> let in_building = x; let new_map = y; ()
            |_    -> failwith "impossible"
          );
            if in_building then
              (match help_disprove players (make_suggestion dest ai) with
                |None   ->
                |Some c ->
              )
             (*call the users in turn to try and disprove the suggestion, if no
             one can, then it'll return None*)
            (*TODO: replace ai with updated ai*)
           else if easy_want_to_accuse then make_accusation state ai else None;


  |Medium-> failwith "unimplemented"
  |Hard->failwith "unimplemented"
);
{
  counter=state.counter+1;
  game_complete= game_complete;
  map=state.map;
  user=state.user;
  ais=state.ais;
  fact_file=state.fact_file;
  dictionary=state.dictionary;
}
else
  state

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
let disprove ai (guess: case_file) =
  match ai.difficulty with
  |Easy   -> easy_helper_reveal ai.hand guess
  |Medium ->failwith "unimplemented"
  |Hard   ->(*if one of the cards has already been in a past guess, the ai wants to
   show that one so we give the other players as little information as possible.
 *)
    failwith "unimplemented"
