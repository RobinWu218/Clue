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

(* [get_first_possible_who possible] takes a list of possible cards and returns
 * the first prof that is in the list. *)
let rec get_first_possible_who possible =
  match possible with
  |[]   -> failwith "there are no possible professors"
  |h::t -> if (((int_of_card h) > (-1)) && ((int_of_card h) < 6))
    then card_to_string h else get_first_possible_who t

(* [get_first_possible_who possible] takes a list of possible cards and returns
 * the first language that is in the list. *)
let rec get_first_possible_with_what possible =
  match possible with
  |[]   -> failwith "there are no possible languages"
  |h::t -> if (((int_of_card h) > 14) && ((int_of_card h) < 21))
    then card_to_string h else get_first_possible_with_what t

(* [get_first_possible_where possible] takes a list of possible cards and
 * returns the first building that is in the list. *)
let rec get_first_possible_where possible =
  match possible with
  |[]   -> failwith "there are no possible buildings"
  |h::t -> if (((int_of_card h) > 5) && ((int_of_card h) < 15))
    then card_to_string h else get_first_possible_where t

(* [get_random_who possible] takes a list of possible cards and returns the
 * a random prof that is in the list. *)
let rec get_random_who possible =
  let i = Random.int 6 in
  if List.mem (Prof (prof_of_int i)) possible then prof_of_int i
  else get_random_who possible

(* [get_random_with_what possible] takes a list of possible cards and returns
 * the a random language that is in the list. *)
let rec get_random_with_what possible =
  let i = Random.int 6 in
  if List.mem (Language (lang_of_int i)) possible then lang_of_int i
  else get_random_with_what possible

(* [get_random_where possible] takes a list of possible cards and returns the
 * a random building that is in the list. *)
let rec get_random_where possible =
  let i = Random.int 9 in
  if List.mem (Building (building_of_int i)) possible then building_of_int i
  else get_random_where possible

(*[replace_ai_with_new new_ai ai_list] returns an updated list of ais,replacing
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

(* [all_no lst] returns true if lst is a list of all no's and returns false
 * otherwise. *)
let rec all_no lst : bool =
  match lst with
  | [] -> true
  | h::t -> h = `N && all_no t

(* [one_yes lst] returns true if lst has at least one yes and false
 * otherwise. *)
let rec one_yes lst : bool =
  match lst with
  | [] -> false
  | h::t -> h = `Y || one_yes t

(* [update_pc_helper_no] updates pc_ref so that any card that no one has
 * becomes the only card of its kind remaining in possible_cards. *)
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

(* [update_pc_helper_yes] updates pc_ref so that any card that at least one
 * player has gets removed from possible_cards. In fact, at most one player
 * can have any one card. *)
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

let update_pc_helper x y a : ai =
  let pc_ref = ref a.possible_cards in
  update_pc_helper_no x y a pc_ref;
  update_pc_helper_yes x y a pc_ref;
  {a with possible_cards = !pc_ref}

(* [update_possible_cards a] checks if any card can be removed from ai [a]'s
 * possible_cards or determined to be in the case file and returns the updated
 * ai. Specifically, any card that at least one player has gets removed from
 * possible_cards, and any card that no one has becomes the only card of its
 * kind remaining in possible_cards and thus must be in the case file. *)
let update_possible_cards (a:ai) : ai =
  a |> update_pc_helper 0 5 (* prof *)
    |> update_pc_helper 6 14 (* building *)
    |> update_pc_helper 15 20 (* language *)

(*[accuse a s] produces a state where the accusation has been made
 * with the case_file that the ai believes is correct. Does not depend on ai
 * difficulty. The ai only makes an accusation
 * when it has narrowed down the possible cards to 3. *)
let accuse (a:ai) (s:state) : bool * state =
  let where      = get_first_possible_where a.possible_cards in
  let who        = get_first_possible_who   a.possible_cards in
  let with_what  = get_first_possible_with_what  a.possible_cards in
  let accusation = {who=who; where=where; with_what=with_what} in
  print_info ("Prof. "^a.character^" is making an accusation:") true;
  print_case_file accusation;
  if accusation = s.fact_file
  then
    begin
      print_important 
        "Uh oh, the AI has won! Their accusation was correct." 
        true;
      print_important "You have lost this game. :(" true;
      print_info
        "CLUE will exit automatically. Feel free to play again!"
        true;
    (true, {s with game_complete=true})
    end
  else
    begin
      print_info "The AI has made the wrong accusation!" true;
      print_important "This AI is now out of of the game." true;
      print_info 
        "Although it can still disprove, it can no longer move or win."
        true;
      print_results
        "Moved to the building it accused to clear the hallway."
        true;
      let new_map     = teleport_professor s.map a.character where in 
      let new_ai      = {a with is_in_game=false} in
      let new_ai_list = replace_ai_with_new new_ai s.ais in
      let new_pg      = (accusation, a.character, None)>::s.past_guesses in
      (true, {s with map = new_map; ais = new_ai_list; past_guesses = new_pg})
    end

(* [accuse_or_not_middle a s] decides whether to accuse or not in the middle of
 * an AI's turn
 * AI logic:
 *   - Easy:   when the number of possible cards is down to 9, the ai just take
 * a random guess and guesses the first who, what, where in the possible cards
 *   - Medium: accuses when he has narrowed the possible cards down to only 3
 *   - Hard:   updates possible cards in ai based on card_status and decides
 * to accuse when there are only three possible cards.
 *)
let accuse_or_not_middle (a:ai) (s:state) : state =
  match a.difficulty with
  | Easy   ->
    if (List.length a.possible_cards)<6 then
      let (accused, new_s) = accuse a s in new_s
    else s
  | Medium ->
    if want_to_accuse a then
      let (accused, new_s) = accuse a s in new_s
    else s
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
        print_info (
          "Prof. "^a.character^" does not wish to make an accusation right now.")
          true;
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
      let ai = List.find (fun a -> a.character = p) s.ais in
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
            print_results "You revealed the card:" true;
            ANSITerminal.(
              print_string []
              (sprintf card_style "  %-68s" (string_of_card card));
              print_endline "");
          Some (p, card)
          end
      | None -> disprove_loop ncurrent (n+1) guess s
      end
  | `No ->
      disprove_loop ncurrent (n+1) guess s

(* [suggest_helper a s] allows ai [a] to make a suggestion and calls 
 * [User.disprove] and [Ai.disprove] until [a] is disproved or not disproved
 * in the end. Calls [teleport_professor] to move the suggested prof's 
 * corresponding ai player to the suggested building and change 
 * that ai's [was_moved] field to true.
 * Requires: ai [a] is currently in a building.
 * Side effects: updates [card_status] of [a]. *)
let suggest_helper (a:ai) (s:state) : state =
  print_info 
    ("Prof. "^a.character^" is making a suggestion about the current building.")
    true;
  let where_option = get_current_building s.map a.character in 
  match where_option with
  | Some where ->
    let (who, with_what) =
      begin
      match a.difficulty with
      | Easy ->
          (prof_of_int (Random.int 6), lang_of_int (Random.int 6))
      | Medium ->
          (get_random_who a.possible_cards,
           get_random_with_what a.possible_cards)
      | Hard ->
          begin
          match Random.int 10 with 
          | i when i < 9 ->
              (get_random_who a.possible_cards,
               get_random_with_what a.possible_cards)
          | _ ->
              (prof_of_int (Random.int 6), lang_of_int (Random.int 6))
          end
      end in
    let guess =
      {who = who;
       where = where;
       with_what = with_what} in
    print_info " " true;
    print_info "The suggestion is:" true;
    print_case_file guess;
    wait_for_user ();
    let (moved_or_not, map) =
      if get_current_building s.map who <> (Some where) 
      then (true, (teleport_professor s.map who where))
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
                      past_guesses =
                      (guess, a.character, Some p)>::news'.past_guesses;} in
        accuse_or_not_middle a news''
        end
    | None ->
        print_results (
          "No one can disprove Prof. "^a.character^"'s suggestion.")
          true;
        let news'' =
          {news' with past_guesses =
                      (guess, a.character, None)>::news'.past_guesses} in
        accuse_or_not_middle a news''
    end
  | None -> failwith "This should not happen in suggest in ai.ml"

(* [suggest a s] is the updated state after ai [a] finishes its turn 
 * suggesting. This updates [possible_cards] by calling [update_possible_cards] 
 * for the hard ai and calls [suggest_helper] to finish the turn. *)
let suggest (a:ai) (s:state) : state =
  match a.difficulty with
  | Easy | Medium -> 
      suggest_helper a s
  | Hard ->
      let new_ai = update_possible_cards a in
      let ai_list = replace_ai_with_new new_ai s.ais in
      let new_s = {s with ais = ai_list} in
      suggest_helper new_ai new_s

(* [top_three lst] returns a random value from the first three elements of the
 * list. If the list is only one element long, then that element is returned.If
 * it is two elements long, then an element is randomly selected.
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

(*[check_building bop b] returns true if bop and is NOT the same as Some b.
 * Returns true otherwise*)
let check_building bop b =
  bop <> Some b

(*[move_where_easy a bop s] returns a building. It decides on this building by
 * randomly choosing one of closest three buildings to the ai.
*)
let rec move_where_easy (a:ai) (bop:building option) (s:state) : building =
  let b = snd (top_three (closest_buildings s.map a.character)) in
  if check_building bop b then b
  else move_where_easy a bop s

(*[move_where_medium_helper lst a bop s] returns a buildling. Returns the
 * closest building to the ai.
*)
let rec move_where_medium_helper (lst:building list)
                        (a:ai) (bop:building option) (s:state) : building =
  match lst with
  | []   -> move_where_easy a bop s
  | h::t -> if check_building bop h then h
            else move_where_medium_helper t a bop s

(* [move_where_medium] returns a buildling. It decides on this
 * building by choosing the cloest building to the ai.
*)
let move_where_medium (a:ai) (bop:building option) (s:state) : building =
  move_where_medium_helper (card_lst_to_building_lst a.possible_cards) a bop s

(*[move_where_hard_helper possible close a bop s] returns a building.
 * It decides on what to return by checking for the closest building to the ai
 * which is also in the a.possible_cards. *)
let rec move_where_hard_helper (possible:building list)
                               (close:building list)
                               (a:ai) (bop:building option) (s:state)
                               : building =
  match close with
  | []   -> failwith "This should not happen in move_where_hard_helper in Ai"
  | [b]  -> if check_building bop b then b else move_where_easy a bop s
  | h::t -> if List.mem h possible && check_building bop h then h
            else move_where_hard_helper possible t a bop s

(*[move_where_hard a bop s] returns the building to move to that is the closest
 * in a.possible_cards. *)
let move_where_hard (a:ai) (bop:building option) (s:state) : building =
  move_where_hard_helper (card_lst_to_building_lst a.possible_cards)
                         (List.map snd (closest_buildings s.map a.character))
                         a bop s

(*[move_where a bop s] returns the building to move to depending on the
 * difficulty of the ai while ensuring that the building is not blocked.
*)
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
      print_results 
        ("Prof. "^a.character^" cannot move anymore.")
        true;
      if is_in_building s.map a.character
      then suggest a s
      else s
    end
  else
    let b = move_where a bop s in
    let (in_building, new_map) =
      move_towards_building s.map a.character b bop n in
      if in_building then
        suggest a {s with map = new_map}
      else
        {s with map = new_map}

(* [get_exit a b s] is the id of an exit to building [b] selected by ai [a].*)
let rec get_exit (a:ai) (b:building) (s:state) : int =
  let exits = List.assoc b s.map.exits in
  let id =
    match List.length exits with
    | 1 -> 1
    | 2 -> Random.int 1 + 1
    | 4 -> Random.int 4 + 1
    | _ -> failwith "This should not happen in get_exit in Ai given map.json"
  in
  if not (is_coord_blocked s.map (List.assoc id exits)) then id
  else get_exit a b s

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
  print_info ("Prof. "^a.character^" used the secret passage.") true;
  let map = use_secret_passage s.map a.character in
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
  | Hard   ->
      if want_to_secret a b then use_secret a s
      else suggest a s

(* [secret_or_roll a b s] allows ai [a] to choose between using the secret
 * passage to leave building [b] and rolling the dice to move out, and returns
 * the updated state. *)
let secret_or_roll (a:ai) (b:building) (s:state) : state =
  match a.difficulty with
  | Easy   ->
    begin
    match Random.int 1 with
      | 0 -> use_secret a s
      | 1 -> leave_and_move a b s
      | _ -> failwith "will never be called: secret or roll"
    end
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
 * action the ai should proceed with given that it was moved there
 * involuntarily.
 * The ai decides this based on whether or not the building that the ai is in
 * currently has a secret passage and whether or not the building that the ai
 * is in right now is blocked. *)
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
 * action the ai should proceed with given that it moved there voluntarily.
 * The ai decides this based on whether or not the building that the ai is in
 * currently has a secret passage and whether or not the building that the ai
 * is in right now is blocked. *)
let in_building_voluntarily (a:ai) (b:building) (s:state) : state =
  let secret = has_secret_passage s.map b in 
  let blocked = is_building_blocked s.map b in 
  match secret, blocked with
  | true,  true  ->
      use_secret a s
  | true,  false ->
      secret_or_roll a b s
  | false, true  ->
      print_info ("Prof. "^a.character^" has to wait until next turn.") true;
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
      if is_coord_blocked s1.map c then s1 else
      move a (roll_two_dice ()) None s1

