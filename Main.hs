-- Main.hs
module Main where

import Board
import System.IO

-- Main function to start the game
main :: IO ()
main = do
    let board = mkBoard 15
    putStrLn "Welcome to the Omok Game!"
    playGame board mkPlayer

-- Function to handle the game loop
playGame :: Board -> Player -> IO ()
playGame board player = do
    -- Display the board
    putStrLn $ boardToStr playerToChar board

    -- Check if the game is over
    if isGameOver board
        then putStrLn "Game over!" >> announceResult board
        else do
            -- Read the next move
            putStrLn $ "Player " ++ show player ++ "'s turn."
            move <- readXY board player
            let newBoard = mark (fst move) (snd move) board player

            -- Switch player and continue the game
            playGame newBoard (if player == mkPlayer then mkOpponent else mkPlayer)

-- Convert a player to a character for display
playerToChar :: Player -> Char
playerToChar p
    | p == mkPlayer = 'O'
    | p == mkOpponent = 'X'
    | otherwise = '.'

-- Read a pair of indices (x, y) from the user
readXY :: Board -> Player -> IO (Int, Int)
readXY board player = do
    -- Implement logic to read and validate user input
    undefined

-- Announce the result of the game
announceResult :: Board -> IO ()
announceResult board
    | isWonBy board mkPlayer = putStrLn "Player 1 wins!"
    | isWonBy board mkOpponent = putStrLn "Player 2 wins!"
    | otherwise = putStrLn "It's a draw!"

