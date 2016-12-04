open Data
open Gmap
open Logic

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
  {
  character      = p;
  hand           = h;
  difficulty     = d;
  was_moved      = false;
  is_in_game     = true;
  possible_cards = possible;
  card_status    = status;
  }

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

(*[want_to_accuse] is true when there are only three possible cards left
 * so the ai knows the right answer and thus wants to accuse. *)
let want_to_accuse ai : bool =
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

(* [all_no lst] returns true if lst is a list of all no's and returns false
 * otherwise. *)
let rec all_no lst : bool =
  match lst with
  | [] -> true
  | h::t -> h = `N && all_no t

(*[one_yes lst] returns true if lst has only one yes and false otherwise. *)
let rec one_yes lst : bool =
  match lst with
  | [] -> false
  | h::t -> h = `Y || one_yes t

(*[update_pc_helper_no] returns a unit. It updates pc_ref. TODO more*)
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
      ANSITerminal.(
      print_string [red] ("Uh oh, the AI has won! That accusation was correct.\n");
      print_string [red ] "You have lost this game. :(\n";
      print_string [red ] ("CLUE will exit automatically. Feel free to play again!\n");
    );
    (true, {s with game_complete=true})
    end
  else
    begin
      ANSITerminal.(
      print_string [red] ("The AI has made the wrong accusation!\n");
      print_string [Bold] ("This AI is now out of of the game.\n");
      print_string [red] ("Although it can still disprove, it can no longer move or win.\n");
      print_string [red] ("As punishment, it is forever stuck in the building it accused...\n");
    );
    let new_map     = teleport_professor s.map a.character where in (*Gmap*)
    let new_ai      = {a with is_in_game=false} in
    let new_ai_list = replace_ai_with_new new_ai s.ais in
    let new_pg      = (accusation, a.character, None)>::s.past_guesses in
    (true, {s with map = new_map; ais = new_ai_list; past_guesses = new_pg})
    end

(* [accuse_or_not_middle a s] decides whether to accuse or not in the middle of
 * an AI's turn
 * AI logic:
 *   - Easy:   when the number of possible cards is down to 9, the ai just takes
 * a random guess and guesses the first who, what, where in the possible cards
 *   - Medium: accuses when he has narrowed the possible cards down to only 3
 *   - Hard:   updates possible cards in ai based on card_status and decides
 * to accuse when there are only three possible cards.
 *)
let accuse_or_not_middle (a:ai) (s:state) : state =
  match a.difficulty with
  | Easy   ->
    if (List.length a.possible_cards)<6 then let (accused, new_s) = accuse a s in new_s
      else s
  | Medium -> if want_to_accuse a then let (accused, new_s) = accuse a s in new_s else s
  | Hard ->
    let new_ai = update_possible_cards a in
    let ai_list = replace_ai_with_new new_ai s.ais in
    let new_s = {s with ais=ai_list} in
    if want_to_accuse new_ai then
    let (accused', new_s') = accuse new_ai new_s in new_s' else new_s

(* [accuse_or_not_start a s] decides whether to accuse or not at the start of
 * an AI's turn
 * AI logic:
 *   - Easy:   simply not accuse and proceed
 *   - Medium: simply not accuse and proceed
 *   - Hard:   checks whether we want to accuse and then proceeds *)
let accuse_or_not_start (a:ai) (s:state) : bool * state =
  match a.difficulty with
  | Easy | Medium   ->
      begin
      Printf.printf "Prof. %s does not wish to make an accusation right now.\n"
                    a.character;
      (false, s)
      end
  | Hard   -> if want_to_accuse a then accuse a s else (false, s)

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
          Printf.printf "You revealed the card %s to disprove the suggestion.\n"
                        (string_of_card card);
          Some (p, card)
          end
      | None -> disprove_loop ncurrent (n+1) guess s
      end
  | `No ->
      disprove_loop ncurrent (n+1) guess s

(*called move towards building while still in building*)
(* AI logic easy: random. *)
(* AI logic medium and hard: only guesses possible cards, not the ones that
 * it knows exists.*)
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
      | Easy ->
          (prof_of_int (Random.int 6), lang_of_int (Random.int 6))
      | Medium | Hard ->
          (easy_helper_who a.possible_cards,
           easy_helper_what a.possible_cards)
      end in
    let guess =
      {who = who;
       where = where;
       with_what = with_what} in
    print_endline "\nThe suggestion is:";
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
          else {a with possible_cards =
            List.filter (fun c' -> c' <> c) a.possible_cards})
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

(* moves to the closest building possible*)
let rec move_where_medium_helper (lst:building list)
                        (a:ai) (bop:building option) (s:state) : building =
  match lst with
  | []   -> move_where_easy a bop s
  | h::t -> if check_building bop h then h
            else move_where_medium_helper t a bop s

(* one of possible buildings if any, otherwise one of closest three
 * cannot possibly be blocked*)
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

(*only moves to possible cards*)
let move_where_hard (a:ai) (bop:building option) (s:state) : building =
  move_where_hard_helper (card_lst_to_building_lst a.possible_cards)
                         (List.map snd (closest_buildings s.map a.character))
                         a bop s

(*ensures that the building is not blocked*)
let move_where (a:ai) (bop:building option) (s:state) : building =
  match a.difficulty with
  | Easy   -> move_where_easy a bop s
  | Medium -> let b = move_where_medium a bop s in
              if not (is_building_blocked s.map b) then b
              else move_where_easy a bop s
  | Hard   -> let b1 = move_where_hard a bop s in
              if not (is_building_blocked s.map b1) then b1
              else let b2 = move_where_medium a bop s in
              if not (is_building_blocked s.map b2) then b2
              else move_where_easy a bop s

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
      move_towards_building s.map a.character b bop n in (* Gmap *)
    if in_building then
      suggest a {s with map = new_map}
    else
      {s with map = new_map}

(* [get_exit a b s] is the id of an exit to building [b] selected by ai [a]. *)
let rec get_exit (a:ai) (b:building) (s:state) : int =
  let exits = List.assoc b s.map.exits in
  match List.length exits with
  | 1 -> 1
  | 2 -> Random.int 1 + 1
  | 4 -> Random.int 4 + 1
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

(*[want_to_secret a b] is true if we use the secret passage, we would reach
 * one of the buildings in a.possible_cards. *)
let want_to_secret a b=
    let dest =
      match b with
      |"Baker"    -> "Rhodes"
      |"Olin"     -> "Duffield"
      |"Duffield" -> "Olin"
      |"Rhodes"   -> "Baker"
      |_-> failwith "only the first four have secret exits"
    in List.mem (Building dest) a.possible_cards

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
  | Hard   -> (*uses the secret passage when the target building is in possible*)
      if want_to_secret a b then use_secret a s
      else suggest a s

(* [secret_or_roll a b s] allows ai [a] to choose between using the secret
 * passage to leave building [b] and rolling the dice to move out, and returns
 * the updated state. *)
let secret_or_roll (a:ai) (b:building) (s:state) : state =
  match a.difficulty with
  | Easy   -> use_secret a s
  | Medium ->
    begin
    match Random.int 1 with
      | 0 -> use_secret a s
      | 1 -> leave_and_move a b s
      | _ -> failwith "will never be called: secret or roll"
    end
  | Hard   -> if want_to_secret a b then use_secret a s
      else leave_and_move a b s

(* [suggest_or_roll a s] allows ai [a] to choose between making a suggestion
 * and rolling the dice to move out, and returns the updated state. *)
let suggest_or_roll (a:ai) (b:building) (s:state) : state =
  match a.difficulty with
  | Easy   -> suggest a s
  | Medium ->
    begin
    match Random.int 1 with
      | 0 -> suggest a s
      | 1 -> leave_and_move a b s
      | _ -> failwith "will never be called: suggest or roll"
    end
  | Hard   -> if List.mem (Building b) a.possible_cards
              then leave_and_move a b s else suggest a s

(* [secret_or_roll_or_suggest a s] allows ai [a] to choose to use the secret
 * passage, or roll the dice to move out, or make a suggestion, and returns
 * the updated state. *)
let secret_or_roll_or_suggest (a:ai) (b:building) (s:state) : state =
  match a.difficulty with
  | Easy   -> suggest a s
  | Medium ->
    begin
    match Random.int 2 with
      | 0 -> suggest a s
      | 1 -> leave_and_move a b s
      | 2 -> use_secret a s
      | _ -> failwith "will never be called: secret or roll or suggest"
    end
  | Hard   -> if want_to_secret a b
                then use_secret a s
              else if List.mem (Building b) a.possible_cards
                then leave_and_move a b s
              else suggest a s

(*[in_building_involuntarily a b s] returns a state after determining what
 * action the ai should proceed with given that it was moved there involuntarily.
 * The ai decides this based on whether or not the building that the ai is in
 * currently has a secret passage and whether or not the building that the ai is
 * in right now is blocked. *)
let in_building_involuntarily (a:ai) (b:building) (s:state) : state =
  let secret = has_secret_passage s.map b in
  let blocked = is_building_blocked s.map b in
  let news =
    begin
    match secret, blocked with
    | true,  true  ->
        suggest_or_secret a b s
    | true,  false ->
        secret_or_roll_or_suggest a b s
    | false, true  ->
        suggest a s
    | false, false ->
        suggest_or_roll a b s
    end
  in assign_was_moved news a.character false

(*[in_building_voluntarily a b s] returns a state after determining what
 * action the ai should proceed with given that it was moved there voluntarily.
 * The ai decides this based on whether or not the building that the ai is in
 * currently has a secret passage and whether or not the building that the ai is
 * in right now is blocked. *) (*TODO please improve*)
let in_building_voluntarily (a:ai) (b:building) (s:state) : state =
  let secret = has_secret_passage s.map b in (* Gmap *)
  let blocked = is_building_blocked s.map b in (* Gmap *)
  match secret, blocked with
  | true,  true  ->
      Printf.printf "Prof. %s has to use the secret passage.\n" a.character;
      use_secret a s (*TODO*)
  | true,  false ->
      print_endline "There is a secret passage available.\n";
      secret_or_roll a b s
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
      let c = get_current_location s1.map a.character in
      if is_exit_blocked s1.map c then s1 else
      move a (roll_two_dice ()) None s1 (* Gmap *)
