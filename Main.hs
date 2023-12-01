-- Main.hs
module Main where

import Board
import System.IO
import System.Random (randomRIO)

-- Main function to start the game
main :: IO ()
main = do
    let board = mkBoard 15
    putStrLn "Welcome to the Omok Game!"
    playGame board mkPlayer

playGame :: Board -> Player -> Bool -> IO ()
playGame board player isComputer = do
    -- Display the board
    putStrLn $ boardToStr playerToChar board

    -- Check if the game is over
    if isGameOver board
        then putStrLn "Game over!" >> announceResult board
        else do
            newBoard <- if isComputer && player == mkOpponent
                            then do
                                putStrLn "Computer's turn."
                                move <- getRandomXY board
                                return $ mark (fst move) (snd move) board player
                            else do
                                putStrLn $ "Player " ++ show player ++ "'s turn (Enter x y coordinates):"
                                move <- readXY board player
                                return $ mark (fst move) (snd move) board player

            -- Switch player and continue the game
            playGame newBoard (if player == mkPlayer then mkOpponent else mkPlayer)

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

-- Generates a random move for the computer opponent
getRandomXY :: Board -> IO (Int, Int)
getRandomXY board = do
    -- implementation as described earlier

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
