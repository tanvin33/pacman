# pacman

An terminal-based pacman game written in OCaml

CS 3110 @ Cornell 

Final Project


# how to play

In order to play the game, download the and unzip the folder. Then navigate to 
that folder in your terminal and run the command "make build" to compile the 
code and then "make play" to run the program.

You will get to a main menu: type in the number of what action you want to take. 

If you choose 1. Play, select a difficulty by typing the number associated with 
it and hitting ENTER/ 

When prompted to give a file name use maze.csv for the default. Otherwise,
put the file name of a csv board of your choice.

Use the WASD keys to move the Pac-Man character "C" around the game board. 
Press p to pause the game or q to quit back to the main menu. 

For the best experience, we suggest making the terminal full screen. 

To win collect all the coins ".". Collcting "*" will make you enter the scatter
state. During this time, the ghosts cannot eat pacman. Otherwise, you lose a 
life when you run into a ghost. You have 3 lives before you lose. 
