open Logic

(* This file prints the game description and rules, prompts the user to choose 
 *   the number of AI bots and level of difficulty, and calls Game.main to 
 *   start the game. The reason this file is factored out from [game.ml] is 
 *   for testing purposes: if we're going to unit test the [Game] module, it 
 *   can't itself invoke its [main] function; if it did, the game would try to 
 *   launch and would interfere with OUnit.
 *)
let () =
  print_endline (
  	"\n\nWelcome to the text-based detective game CLUE developed by \n"^
  	"David Zhao, Karen Fang, Robin Wu, and Alice Chen for CS 3110. \n"^
    "If you are unfamiliar with the game CLUE, here is the instructions: \n"^
    "http://www.hasbro.com/common/instruct/Clue_(2002).pdf \n\n"^
  	"A computer virus strikes all the computers on Cornell’s campus! \n"^
  	"It only prints out random camel facts... but don’t let that fool you! \n"^
  	"It’s a dangerous virus and the perpetrator needs to be caught. \n"^
  	"It’s your job to answer these three questions: \n"^
  	"Who did it? Where? And with what language? \n"^
  	"Whoever makes the correct accusation first wins the game.\n\n"^
  	"6 Professors: Anne Bracy, Michael Clarkson, Daisy Fan, \n"^
  	"              David Gries, Joe Halpern, Walker White\n"^
  	"9 Buildings:  Baker Hall, Carpenter Hall, Duffield Hall, \n"^
  	"              Gates Hall, Klarman Hall, Olin Hall, \n"^
  	"              Phillips Hall, Rhodes Hall, Statler Hall\n"^
  	"6 Languages:  Bash, C, Java, MATLAB, OCaml, Python\n");
  print_endline 
    "Please enter the number of AI bots you want to play against (2-5).";
  let num_AI = get_choice_num_ai () in
  print_endline 
    "Please enter the level of difficulty (1 easy, 2 medium, 3 hard).";
  let dlevel = get_choice_three () in
  Game.main num_AI dlevel

