open Logic
open Data

(* This file prints the game description and rules, prompts the user to choose 
 *   the number of AI bots and level of difficulty, and calls Game.main to 
 *   start the game. 
 *)
let () =
  ANSITerminal.resize  70 50;
  print_info (
    "+--------------------------------------------------------------------+\n"^
    "|    Welcome to the text-based detective game CLUE developed by      |\n"^
    "|   David Zhao, Karen Fang, Robin Wu, and Alice Chen for CS 3110.    |\n"^
    "|            If you are unfamiliar with the game CLUE,               |\n"^
    "|                  here are the instructions:                        |\n"^
    "|       http://www.hasbro.com/common/instruct/Clue_(2002).pdf        |\n"^
    "+--------------------------------------------------------------------+\n|")
    false;
  ANSITerminal.(print_string [Bold; white; on_black] 
    " The story...                                                       ");
  print_info ("|\n"^
    "| A computer virus strikes all the computers on Cornell’s campus! It |\n"^
    "| only prints out random camel facts... but don’t let that fool you! |\n"^
    "| It is a dangerous virus and the perpetrator needs to be caught.    |\n"^
    "| Your job is to answer these three questions:                       |\n|")
    false;
  ANSITerminal.(print_string [Bold; blue; on_black]
    " Who did it? Where? And with what language?                         ");
  print_info ("|\n"^
    "+--------------------------------------------------------------------+\n|")
    false;
  ANSITerminal.(print_string [Bold; Underlined; yellow; on_black]
    "     Professors      |        Buildings       |      Languages      ");
  print_info ("|\n"^
    "|       Bracy         |          Baker         |        Bash         |\n"^
    "|      Clarkson       |        Carpenter       |         C           |\n"^
    "|        Fan          |         Duffield       |        Java         |\n"^
    "|       Gries         |          Gates         |       MATLAB        |\n"^
    "|      Halpern        |         Klarman        |        OCaml        |\n"^
    "|       White         |           Olin         |       Python        |\n"^
    "|                     |         Phillips       |                     |\n"^
    "|                     |          Rhodes        |                     |\n"^
    "|                     |         Statler        |                     |\n"^
    "+--------------------------------------------------------------------+\n")
    false;
  (* allow players to read the information *)
  wait_for_user ();
  print_info
    "Please enter the number of AI bots you want to play against (2-5)." true;
  let num_AI = get_choice_num_ai () in
  print_info 
    "Please enter the level of difficulty (1 easy, 2 medium, 3 hard)." true;
  let dlevel = get_choice_three () in
    Game.main num_AI dlevel

