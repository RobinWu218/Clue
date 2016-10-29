type prof = Bracy | Clarkson | Fan | Gries | Halpern | White
type building = ...
type language = …
type card = prof | building | language
type case_file = {who:prof; where:building; with:language}


(*
6 Professors:       
Anne Bracy 0, Michael Clarkson 1, Daisy Fan 2, David Gries 3, Joe Halpern 4, Walker White 5
9 Buildings:         
Baker Hall 6, Carpenter Hall 7, Duffield Hall 8, Gates Hall 9, Klarman Hall 10, Olin Library 11, Phillips Hall 12, Rhodes Hall 13, Statler Hall 14
6 Languages:       
Bash 15, C 16, Java 17, MATLAB 18, OCaml 19, Python 20
*)
let card_of_int (n:int) : card = 
let int_of_card (c:card) : int =


(***** helpers for init *****)


(* [generate_case_file ()] is the case file containing the answers to the questions: Who? Where? What language? *)
let generate_case_file () : case_file =


(* [assign_characters n] is an n-tuple of non-repeating profs.
 * Requires: [n] is an integer between 3 and 6 inclusive. *)
let assign_characters (n:int) =


(* [deal_card n] is an n-tuple of non-repeating card lists. 
 * Requires: [n] is an integer between 3 and 6 inclusive. *)
let deal_card (n:int) =


(* [init n d] is the initial game state with [n] AI bots and a difficulty level of [d]. It prints out which character each player plays, and the cards in the user’s hands.
 * Requires: [n] is an integer between 2 and 5 inclusive, [d] is an integer between 1 and 3 inclusive. *)
let init_state (n:int) (d:int) : state = 
    if n = 2 then 
        let (char1,char2,char3) = assign_characters n in
        let (cards1,cards2,cards3) = deal_cards n in 
        {map = ...; n = n; players = (char1,char2,char3); user = {character = char1; turn = int_of_card char1; ...}; AI1 = Some ...; AI2 = Some ...; AI3 = None, ...}
    else if n = 3 then ...
    else if n = 4 then ...
    else if n = 5 then ...
    else failwith “This should not happen in init_state”


(***** helpers for repl *****)


(* [roll_two_dice ()] is the sum of two random integers between 1 and 6 inclusive. It also prints the two integers and the sum. *)
let roll_two_dice () =
    let d1 = 1 + Random.int 5 in
    let d2 = 1 + Random.int 5 in
    let sum = d1 + d2 in
    print_endline “Die 1: ” ^ (string_of_int d1);
    print_endline “Die 2: ” ^ (string_of_int d2);
    print_endline “# of movements: ” ^ (string_of_int sum);


(* [repl turn s] is . *)
let rec repl (turn:int) (s:state) : unit = 
    if (turn mod s.n) = 0 then ...
    else if (turn mod s.n) = 1 then ...
    else if (turn mod s.n) = 2 then ...
    ...
    repl (turn + 1) s’


(***** main *****)


(* [main n d] is the main entry point from outside this module to initialize a game with [n] AI bots and a difficulty level of [d] and start playing it. *)
let main n d =
  let s = init n d in 
  print_endline "The game begins now... ";
  print_endline "To play the game, type short phrases into the command line when it is your turn. You may terminate the game at any time with [quit], show the current map with [map], show your cards with [cards], and redisplay instructions with [help]. \n";
  repl o s