Open Game

(* This file prints the game description and rules, prompts the user to choose 
 *   the number of AI bots and level of difficulty, and calls Game.main to 
 *   start the game. *)
let () =
  Print_endline (
  	"\n\nWelcome to the text-based detective game CLUE developed by \n"^
  	"Karen Fang, David Zhao, Robin Wu, and Alice Chen for CS 3110. \n"^
  	"A computer virus strikes all the computers on Cornell’s campus! \n"^
  	"It only prints out random camel facts... but don’t let that fool you! \n"^
  	"It’s a dangerous virus and the perpetrator needs to be caught. \n"^
  	"It’s your job to answer these three questions: \n"^
  	"Who did it? Where? And with what language? \n"^
  	"Whoever makes the correct accusation first wins the game.\n\n"^
  	"6 Professors: Anne Bracy, Michael Clarkson, Daisy Fan, \n"^
  	"              David Gries, Joe Halpern, Walker White\n"^
  	"9 Buildings:  Baker Hall, Carpenter Hall, Duffield Hall, \n"^
  	"              Gates Hall, Klarman Hall, Olin Library, \n"^
  	"              Phillips Hall, Rhodes Hall, Statler Hall"^
  	"6 Languages:  Bash, C, Java, MATLAB, OCaml, Python");
  print_endline 
    "Please enter the number of AI bots you want to play against (2-5).\n";
  print_string  "> ";
  let num_AI = read_int () in (* TODO prereq and robust *)
  print_endline 
    "Please enter the level of difficulty (1 easy, 2 medium, 3 hard).\n";
  print_string  "> ";
  let dlevel = read_int () in (* TODO prereq and robust *)
  Game.main num_AI dlevel