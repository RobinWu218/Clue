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
    let r_prof = int_to_prof(Random.int 6) in
    let r_building = int_to_building(Random.int 9) in
    let r_lang = int_to_lang(Random.int 6) in
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

(* [deal_card n] is an list of non-repeating cards.
 * Requires: [n] is an integer between 3 and 6 inclusive. 
 *           [card_chosen] is a string list indicating the chosen cards*)
let deal_card card_chosen (n:int) =
    let card_lst = ref [] in
    select_non_repeat_lst (card_lst_to_int_lst card_chosen) card_lst n 21;
    lst_to_card_lst (!card_lst)

(* [init n d] is the initial game state with [n] AI bots and a difficulty level
of [d]. It prints out which character each player plays, and the cards in the
user’s hands.
 * Requires: [n] is an integer between 2 and 5 inclusive, [d] is an integer
between 1 and 3 inclusive. *)
let init_state (n:int) (d:int) : state =
    if n = 2 then
        let character_lst = assign_characters n in
        let card_lst = deal_cards n in
        {map = ...;
         n = n;
         players = (char1,char2,char3);
         user = {character = char1; turn = int_of_card char1; ...};
         AI1 = Some ...;
         AI2 = Some ...;
         AI3 = None, ...}
    else if n = 3 then ...
    else if n = 4 then ...
    else if n = 5 then ...
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