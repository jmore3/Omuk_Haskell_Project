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
            putStrLn $ "Player " ++ show player ++ "'s turn (Enter x y coordinates):"
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
    input <- getLine
    let parsed = reads input :: [(Int, Int)]
    if null parsed
       then putStrLn "Invalid input. Please enter x and y coordinates." >> readXY board player
       else let (x, y) = head parsed
            in if isValidMove x y board
               then return (x, y)
               else putStrLn "Invalid move. Please try again." >> readXY board player

isValidMove :: Int -> Int -> Board -> Bool
isValidMove x y board = 
    x > 0 && x <= size board && y > 0 && y <= size board && isEmpty x y board

-- Announce the result of the game
announceResult :: Board -> IO ()
announceResult board
    | isWonBy board mkPlayer = putStrLn "Player 1 wins!"
    | isWonBy board mkOpponent = putStrLn "Player 2 wins!"
    | otherwise = putStrLn "It's a draw!"
