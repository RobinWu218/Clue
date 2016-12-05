CLUE
Authors: 
  Karen Fang (kf288)
  David Zhao (dfz8)
  Robin Wu   (zw287)
  Alice Chen (xc278)
Last updated: 12/4/2016

## To Compile and Play the Game
Run: make play
For best results, run the command when the terminal window is not full screen.
That way ANSITerminal is able to resize the terminal for the best size.


To Clean the Directory
Run: make clean

Below is a brief summary of important information and rules of the game. You 
may find it helpful to place this text file next to the terminal for instant 
reference while playing the game. It is also almost necessary for you to take 
notes while playing the game since keeping track of all 21 possible cards in 
your head can be really hard.

================================================================================
********************************************************************************

Who did it? Where? And with what language?

+--------------------------------------------------------------------+
|     Professors      |        Buildings       |      Languages      |
+--------------------------------------------------------------------+
|       Bracy         |          Baker         |        Bash         |
|      Clarkson       |        Carpenter       |         C           |
|        Fan          |         Duffield       |        Java         |
|       Gries         |          Gates         |       MATLAB        |
|      Halpern        |         Klarman        |        OCaml        |
|       White         |           Olin         |       Python        |
|                     |         Phillips       |                     |
|                     |          Rhodes        |                     |
|                     |         Statler        |                     |
+--------------------------------------------------------------------+

Making a wrong suggestion does not cost you anything, but you instantly lose if 
you make a wrong accusation, so do not accuse unless you are quite sure about 
your accusation. 

The character you are playing can also be the culprit so feel free to suggest 
or accuse your own character whenever you see fit.

Making a suggestion about any professor will result in teleporting that 
professor to the building in the suggestion no matter where the professor was 
prior to that.

If an AI makes a wrong accusation, that AI loses but still stays in the game 
to disprove other players’ suggestions.

When it is your turn to disprove another player's suggestion, you have to 
reveal a card if you have at least one card in the suggested three cards.

You may not leave and re-enter the same building in a single turn. You may do 
so across different turns, though.

** For full instructions: **
http://www.hasbro.com/common/instruct/Clue_(2002).pdf

Things that are different from rules described in the instruction booklet: 
1. Players are allowed to enter the same square twice on the same turn.
2. Player have to use up all steps before ending their turns unless they enter 
   a building.
3. Unlike in the board game where each weapon is moved around when being 
   suggested or accused, languages do not show up on our map.

********************************************************************************
================================================================================

