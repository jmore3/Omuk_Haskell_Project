# Omuk_Haskell_Project
A simple API to play the Omok game. 

Board.hs (Game Model)
This module should handle the game logic and state without any I/O operations. It includes functions for creating the game board, manipulating the board state, checking game conditions, and converting the board to a string for display.

Data Structures
Board: A two-dimensional list or an array to represent the 15x15 board.
Player: A simple data type or integer to represent players.
Functions
Creating a board and accessing its elements

mkBoard: Initializes an empty board.
mkPlayer: Creates the first player.
mkOpponent: Creates the second player.
size: Returns the size of the board.
row, column: Return specified row or column.
Checking places and placing stones

mark: Places a stone on the board.
isEmpty, isMarked, isMarkedBy: Check the state of a board cell.
marker: Returns the player who has marked a specific cell.
isFull: Checks if the board is fully marked.
Determining the game outcome

isWonBy: Checks if a player has won.
isDraw: Checks if the game is a draw.
isGameOver: Checks if the game is over.
Converting to a string for printing

boardToStr: Converts the board state into a string for display.
Main.hs (View and Controller)
This module should handle all I/O operations, including interacting with users and displaying the game board.

Functions
Reading user inputs and printing outputs

playerToChar: Returns a character representation of a player.
readXY: Reads a pair of indices from the user.
Playing game

main: The main function to play the game.
