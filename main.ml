open Logic

(* This file prints the game description and rules, prompts the user to choose 
 *   the number of AI bots and level of difficulty, and calls Game.main to 
 *   start the game. The reason this file is factored out from [game.ml] is 
 *   for testing purposes: if we're going to unit test the [Game] module, it 
 *   can't itself invoke its [main] function; if it did, the game would try to 
 *   launch and would interfere with OUnit.
 *)
let () =
  ANSITerminal.resize 98 50;
  ANSITerminal.(
    print_string [yellow] (
    "+--------------------------------------------------------------------+\n"^
    "|    Welcome to the text-based detective game CLUE developed by      |\n"^
    "|   David Zhao, Karen Fang, Robin Wu, and Alice Chen for CS 3110.    |\n"^
    "|            If you are unfamiliar with the game CLUE,               |\n"^
    "|                  here are the instructions:                        |\n"^
    "|       http://www.hasbro.com/common/instruct/Clue_(2002).pdf        |\n"^
    "+--------------------------------------------------------------------+\n|");
    print_string [Bold; white] 
    " The story...                                                       ";
    print_string [yellow] ("|\n"^
    "| A computer virus strikes all the computers on Cornell’s campus! It |\n"^
    "| only prints out random camel facts... but don’t let that fool you! |\n"^
    "| It is a dangerous virus and the perpetrator needs to be caught.    |\n"^
    "| Your job is to answer these three questions:                       |\n|");
    print_string [Bold; red]
    " Who did it? Where? And with what language?                         ";
    print_string [yellow] ("|\n"^
    "+--------------------------------------------------------------------+\n|");
    print_string [Bold; Underlined; yellow]
    "     Professors      |        Buildings       |      Languages      ";
    print_string [yellow] ("|\n"^
    "|       Bracy         |          Baker         |        Bash         |\n"^
    "|      Clarkson       |        Carpenter       |         C           |\n"^
    "|        Fan          |         Duffield       |        Java         |\n"^
    "|       Gries         |          Gates         |       MATLAB        |\n"^
    "|      Halpern        |         Klarman        |        OCaml        |\n"^
    "|       White         |           Olin         |       Python        |\n"^
    "|                     |         Phillips       |                     |\n"^
    "|                     |          Rhodes        |                     |\n"^
    "|                     |         Statler        |                     |\n"^
    "+--------------------------------------------------------------------+\n"));
  (* allow players to read the information *)
  Data.wait_for_user ();
  ANSITerminal.(print_string [red]
    "Please enter the number of AI bots you want to play against (2-5).\n");
  let num_AI = get_choice_num_ai () in
  ANSITerminal.(print_string [red]
    "Please enter the level of difficulty (1 easy, 2 medium, 3 hard).\n");
  let dlevel = get_choice_three () in
    Game.main num_AI dlevel

