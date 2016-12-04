open Data
open Gmap
open Logic

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

(* [init p h d lst] is the AI data structure that represents an AI playing
 * character [p] on difficulty level [d], with hand [h]. [lst] is a list of
 * all players initialized in the game.
 *)
let init (p:prof) (h:hand) (d:difficulty) (lst:prof list) : ai =
  let possible = [
    Prof "Bracy";        Prof "Clarkson";      Prof "Fan";
    Prof "Gries";        Prof "Halpern";       Prof "White";
    Building "Baker";    Building "Carpenter"; Building "Duffield";
    Building "Gates";    Building "Klarman";   Building "Olin";
    Building "Phillips"; Building "Rhodes";    Building "Statler";
    Language "Bash";     Language "C";         Language "Java";
    Language "MATLAB";   Language "OCaml";     Language "Python"] in
  let status = List.map (fun x ->
    let arr = Array.make 21 `Blank in
    let pos = card_lst_to_int_lst h in
    if x = p then
      begin
        List.iter (fun i -> arr.(i) <- `N) (card_lst_to_int_lst possible);
        List.iter (fun i -> arr.(i) <- `Y) pos; (x, arr)
      end
    else
      begin List.iter (fun i -> arr.(i) <- `N) pos; (x, arr) end) lst in
  let a1 = {
    character      = p;
    hand           = h;
    difficulty     = d;
    was_moved      = false;
    is_in_game     = true;
    destination    = None;
    known_cards    = h;
    possible_cards = possible;
    card_status    = status;
  } in
  known_not_possible a1

(* [get_ai p s] is the AI data structure for the AI playing character [p] in
 * state [s].
 * Raises: Not_found if [p] is not represented by an AI in [s].
 *)
let get_ai (p:prof) (s:state) : ai =
  List.find (fun a -> a.character = p) s.ais

(* [get_difficulty a] is the difficulty level of the AI [a].
 *)
let get_difficulty (a:ai) : difficulty =
  a.difficulty

(* [still_in_game a] is [true] iff the AI [a] is still in the game.
 * If that ai is out, s/he can still prove suggestions wrong.
 *)
let still_in_game (a:ai) : bool =
  a.is_in_game

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
 * first building that is in the list. This is a helper function for the easy ai.
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

(*[hard_want_to_accuse] is true when there are 3 cards for where the ai knows
 * that none of the other professors have. Else, it is false.
*)
let hard_want_to_accuse ai : bool = failwith "do soon"


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

(* TODO all `N *)
let rec all_no lst : bool =
  match lst with
  | [] -> true
  | h::t -> h = `N && all_no t

(* TODO one `Y *)
let rec one_yes lst : bool =
  match lst with
  | [] -> false
  | h::t -> h = `Y || one_yes t

(*TODO*)
let update_pc_helper_no (x:int) (y:int) (a:ai)
                        (pc_ref:card list ref) : unit =
  for i = x to y do
    if all_no (List.map (fun (p,arr) -> arr.(i)) a.card_status) then
    pc_ref := List.filter (fun c ->
                   let cint = int_of_card c in
                   not (cint <> i && x <= cint && cint <= y))
                 !pc_ref
    else ()
  done

(*TODO*)
let update_pc_helper_yes (x:int) (y:int) (a:ai)
                         (pc_ref:card list ref) : unit =
  for i = x to y do
    if one_yes (List.map (fun (p,arr) -> arr.(i)) a.card_status) then
    pc_ref := if List.mem (card_of_int i) !pc_ref
              then
                List.filter (fun c -> c <> (card_of_int i)) !pc_ref
              else
                !pc_ref
    else ()
  done

(*TODO*)
let update_pc_helper x y a : ai =
  let pc_ref = ref a.possible_cards in
  update_pc_helper_no x y a pc_ref;
  update_pc_helper_yes x y a pc_ref;
  {a with possible_cards = !pc_ref}

(* TODO checks if any card can be removed from possible_cards or determined
 * to be in the case file and thus known. *)
let update_possible_cards (a:ai) : ai =
  a |> update_pc_helper 0 5
    |> update_pc_helper 6 14
    |> update_pc_helper 15 20

(*[replace_ai_with_new new_ai ai_list] returns an updated list of ais, replacing
the old ai with this new one.*)
let rec replace_ai_with_new new_ai ai_list =
  match ai_list with
  |[]-> failwith "not an ai"
  |h::t-> if h.character=new_ai.character then new_ai::t
          else h::replace_ai_with_new new_ai t

(*[accuse a s] produces a state where the accusation has been made
 * with the case_file that the ai believes is correct. Does not depend on ai
 * difficulty. The ai only makes an accusation
 * when it has narrowed down the possible cards to 3. *)
let accuse (a:ai) (s:state) : bool * state =
  let where      = easy_helper_where a.possible_cards in
  let who        = easy_helper_who   a.possible_cards in
  let with_what  = easy_helper_what  a.possible_cards in
  let accusation = {who=who; where=where; with_what=with_what} in
  Printf.printf "Prof. %s is making an accusation:\n" a.character;
  print_case_file accusation;
  if accusation = s.fact_file
  then
    begin
    print_endline "Uh oh, the AI has won! That accusation was correct.";
    print_endline "You have lost this game. :(";
    print_endline "CLUE will exit automatically. Feel free to play again!";
    (true, {s with game_complete=true})
    end
  else
    begin
    print_endline ("The AI has made the wrong accusation!\n"^
      "This AI is now out of of the game.\n"^
      "Although it can still disprove, it can no longer move or win.\n"^
      "As punishment, it is forever stuck in the building it accused...");
    let new_map     = teleport_professor s.map a.character where in (*Gmap*)
    let new_ai      = {a with is_in_game=false} in
    let new_ai_list = replace_ai_with_new new_ai s.ais in
    let new_pg      = (accusation, a.character, None)>::s.past_guesses in
    (true, {s with map = new_map; ais = new_ai_list; past_guesses = new_pg})
    end

(* TODO decides whether to accuse or not in the middle of an AI's turn
 * AI logic:
 *   - Easy:   simply not accuse and proceed
 *   - Medium: accuses when he has narrowed the possible cards down to only 3
 *   - Hard:   TODO *)
let accuse_or_not_middle (a:ai) (s:state) : state =
  match a.difficulty with
  | Easy   -> s (* will accuse when there are 9 cards left randomly*)
  | Medium -> if easy_want_to_accuse a then accuse a s else s
  | Hard   -> if hard_want_to_accuse a then accuse a s else s

(* TODO decides whether to accuse or not at the start of an AI's turn
 * AI logic:
 *   - Easy:   simply not accuse and proceed
 *   - Medium: simply not accuse and proceed
 *   - Hard:   TODO *)
let accuse_or_not_start (a:ai) (s:state) : bool * state =
  match a.difficulty with
  | Easy | Medium   ->
      begin
      Printf.printf "Prof. %s does not wish to make an accusation right now.\n"
                    a.character;
      (false, s)
      end
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
      match ai_disprove ai guess s.ais with
      | Some card -> Some (p, card)
      | None -> disprove_loop ncurrent (n+1) guess s
      end
  | `User ->
      begin
      match user_disprove s guess s.ais with
      | Some card ->
          begin
          Printf.printf "You revealed the card %s to disprove the suggestion."
                        (string_of_card card);
          Some (p, card)
          end
      | None -> disprove_loop ncurrent (n+1) guess s
      end
  | `No ->
      disprove_loop ncurrent (n+1) guess s


(* AI logic: random. *)
let suggest_easy (a:ai) (s:state) : (prof * language) =
  (prof_of_int (Random.int 6), lang_of_int (Random.int 6))

(* AI logic: only guesses possible cards, not the ones that it knows exists.*)
let suggest_medium (ai:ai) (s:state) : (prof * language) =
  (easy_helper_who ai.possible_cards, easy_helper_what ai.possible_cards)

let suggest_hard (ai:ai) (s:state) :  (prof * language) =
  failwith "todo"

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
        begin
        List.iter (fun (x,arr) ->
          if x = p then
            arr.(int_of_card c) <- `Y
          else
            arr.(int_of_card c) <- `N
          ) a.card_status;
        let newais = List.map (fun a' ->
          if a' <> a then a'
          else known_not_possible {a with known_cards = c>::a.known_cards})
          s.ais in
        let news'' =
          {news' with ais = newais;
                      past_guesses = (*TODO possibly have a helper.ml?*)
                      (guess, a.character, Some p)>::news'.past_guesses;} in
        accuse_or_not_middle a news''
        end
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
 * Requires: lst is non-empty. *)
let top_three (lst:'a list) : 'a =
  match lst with
  | [] ->
      failwith "An empty list is passed into top_three in Ai, wrong!"
  | [elt] ->
      elt
  | [elt1;elt2] ->
      begin
      match Random.int 1 with
      | 0 -> elt1
      | 1 -> elt2
      | _ -> failwith "This should not happen in top_three in Ai"
      end
  | elt1::elt2::elt3::_ ->
      begin
      match Random.int 2 with
      | 0 -> elt1
      | 1 -> elt2
      | 2 -> elt3
      | _ -> failwith "This should not happen in top_three in Ai"
      end

let check_building bop b =
  (* if bop = Some b then false else true
  simplifies to: *)
  bop <> Some b

(* one of closest three buildings *)
let rec move_where_easy (a:ai) (bop:building option) (s:state) : building =
  let b = snd (top_three (closest_buildings s.map a.character)) in
  if check_building bop b then b
  else move_where_easy a bop s

let rec move_where_medium_helper (lst:building list)
                        (a:ai) (bop:building option) (s:state) : building =
  match lst with
  | []   -> move_where_easy a bop s
  | h::t -> if check_building bop h then h
            else move_where_medium_helper t a bop s

(* one of possible buildings if any, otherwise one of closest three *)
let move_where_medium (a:ai) (bop:building option) (s:state) : building =
  move_where_medium_helper (card_lst_to_building_lst a.possible_cards) a bop s

let rec move_where_hard_helper (possible:building list)
                               (close:building list)
                               (a:ai) (bop:building option) (s:state)
                               : building =
  match close with
  | []   -> failwith "This should not happen in move_where_hard_helper in Ai"
  | [b]  -> if check_building bop b then b else move_where_easy a bop s
  | h::t -> if List.mem h possible && check_building bop h then h
            else move_where_hard_helper possible t a bop s

let move_where_hard (a:ai) (bop:building option) (s:state) : building =
  move_where_hard_helper (card_lst_to_building_lst a.possible_cards)
                         (List.map snd (closest_buildings s.map a.character))
                         a bop s

let move_where (a:ai) (bop:building option) (s:state) : building =
  match a.difficulty with
  | Easy   -> move_where_easy a bop s
  | Medium -> move_where_medium a bop s
  | Hard   -> move_where_hard a bop s

(* [move a n bop s] allows the AI [a]'s character to move n steps,
 * or fewer if the character gets into a building
 * before using up all the steps. If [bop] is [Some b'] then user cannot
 * move into [b'] since s/he just left that building in the same turn. *)
let move (a:ai) (n:int) (bop:building option) (s:state) : state =
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
    let b = move_where a bop s in
    let (in_building, new_map) =
      move_towards_building s.map a.character b n in (* Gmap *)
    if in_building then
      suggest a {s with map = new_map}
    else
      {s with map = new_map}

(* [get_exit a b s] is the id of an exit to building [b] selected by ai [a]. *)
let rec get_exit (a:ai) (b:building) (s:state) : int =
  let exits = List.assoc b s.map.exits in
  match List.length exits with
  | 1 -> 1
  | 2 -> begin
         match a.difficulty with
         | Easy   -> Random.int 1 + 1
         | Medium -> failwith "TODO"
         | Hard   -> failwith "TODO"
         end
  | 4 -> begin
         match a.difficulty with
         | Easy   -> Random.int 4 + 1
         | Medium -> failwith "TODO"
         | Hard   -> failwith "TODO"
         end
  | _ -> failwith "This should not happen in get_exit in Ai given map.json"

(* [leave_and_move a b s] is the updated state after ai [a] moves out of
 * building [b].
 * Requires: [a] is currently in building [b]. *)
let leave_and_move (a:ai) (b:building) (s:state) : state =
  let map = leave_building s.map a.character (get_exit a b s) in
  move a (roll_two_dice ()) (Some b) {s with map = map}

(* [use_secret a s] is the updated state after ai [a] uses the secret
 * passage in the current building.
 * Requires: [a] is currently in a building where there is a secret
 *           passage. *)
let use_secret (a:ai) (s:state) : state =
  Printf.printf "Prof. %s used the secret passage.\n" a.character;
  let map = use_secret_passage s.map a.character in (* Gmap *)
  suggest a {s with map = map}

(* [suggest_or_secret a b s] allows ai [a] to choose between making a
 * suggestion and using the secret passage to leave building [b], and returns
 * the updated state. *)
let suggest_or_secret (a:ai) (b:building) (s:state) : state =
  match a.difficulty with
  | Easy   -> suggest a s (* always help user get more info *)
  | Medium ->
      begin
      match Random.int 1 with
      | 0 -> suggest a s
      | 1 -> use_secret a s
      | _ -> failwith "will never be called"
      end
  | Hard   -> failwith "TODO"

(* [secret_or_roll a b s] allows ai [a] to choose between using the secret
 * passage to leave building [b] and rolling the dice to move out, and returns
 * the updated state. *)
let secret_or_roll (a:ai) (b:building) (s:state) : state =
  match a.difficulty with
  | Easy   -> use_secret a s
  | Medium -> failwith "TODO depends on which is easier to get to destination"
  | Hard   -> failwith "TODO" (* leave_and_move a b s *)

(* [suggest_or_roll a s] allows ai [a] to choose between making a suggestion
 * and rolling the dice to move out, and returns the updated state. *)
let suggest_or_roll (a:ai) (b:building) (s:state) : state =
  match a.difficulty with
  | Easy   -> suggest a s
  | Medium -> failwith "TODO depends on which is easier to get to destination"
  | Hard   -> failwith "TODO" (* leave_and_move a b s *)

(* [secret_or_roll_or_suggest a s] allows ai [a] to choose to use the secret
 * passage, or roll the dice to move out, or make a suggestion, and returns
 * the updated state. *)
let secret_or_roll_or_suggest (a:ai) (b:building) (s:state) : state =
  match a.difficulty with
  | Easy   -> suggest a s
  | Medium -> failwith "TODO depends on which is easier to get to destination"
  | Hard   -> failwith "TODO" (* leave_and_move a b s *)

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
        suggest_or_roll a b s (*TODO*)
    end
  in assign_was_moved news a.character false

(*TODO*)
let in_building_voluntarily (a:ai) (b:building) (s:state) : state =
  let secret = has_secret_passage s.map b in (* Gmap *)
  let blocked = is_building_blocked s.map b in (* Gmap *)
  match secret, blocked with
  | true,  true  ->
      Printf.printf "Prof. %s has to use the secret passage.\n" a.character;
      use_secret a s (*TODO*)
  | true,  false ->
      print_endline "There is a secret passage available.";
      secret_or_roll a b s (*TODO*)
  | false, true  ->
      Printf.printf "Prof. %s has to wait until next turn.\n" a.character;
      s
  | false, false ->
      leave_and_move a b s

(* [step a s] peforms a turn for AI player [a]. This involves:
 *   - defining and setting goals by processing knowledge from suggestions and
 *     making deductions about other players' turns.
 *   - moving the AI around the map
 *   - forming and making suggestions/accusations
 * Returns: an updated game state.
 * Requires: ai [a] is still in game, i.e., [a.is_in_game] is [true].
 *)
let rec step (a:ai) (s:state) : state =
  let (end_turn, s1) = accuse_or_not_start a s in
  if end_turn then s1 else
  match get_current_building s1.map a.character with (* Gmap *)
  | Some b ->
      if a.was_moved
      then in_building_involuntarily a b s1
      else in_building_voluntarily a b s1
  | None ->
      move a (roll_two_dice ()) None s1 (* Gmap *)







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
