-- Board.hs
module Board where

-- Define types for the Board, Players, and Stones
type Board = [[Maybe Player]]
type Player = Int
type Stone = Maybe Player

-- Constants for the players
mkPlayer :: Player
mkPlayer = 1

mkOpponent :: Player
mkOpponent = 2

-- Create an empty board of size n x n
mkBoard :: Int -> Board
mkBoard n = replicate n (replicate n Nothing)

-- Return the size of the board
size :: Board -> Int
size = length

-- Return a row of the board
row :: Int -> Board -> [Stone]
row y bd = bd !! (y - 1)

-- Return a column of the board
column :: Int -> Board -> [Stone]
column x bd = map (!! (x - 1)) bd

-- Mark a place on the board by a player
mark :: Int -> Int -> Board -> Player -> Board
mark x y bd p = 
    take (y - 1) bd ++ 
    [replace (bd !! (y - 1)) x p] ++ 
    drop y bd
  where
    replace :: [Stone] -> Int -> Player -> [Stone]
    replace row x p = take (x - 1) row ++ [Just p] ++ drop x row

-- Check if a place on the board is empty
isEmpty :: Int -> Int -> Board -> Bool
isEmpty x y bd = (bd !! (y - 1)) !! (x - 1) == Nothing

-- Check if a place on the board is marked
isMarked :: Int -> Int -> Board -> Bool
isMarked x y bd = not $ isEmpty x y bd

-- Check if a place on the board is marked by a specific player
isMarkedBy :: Int -> Int -> Board -> Player -> Bool
isMarkedBy x y bd p = (bd !! (y - 1)) !! (x - 1) == Just p

-- Return the player who marked a specific cell
marker :: Int -> Int -> Board -> Maybe Player
marker x y bd = (bd !! (y - 1)) !! (x - 1)

-- Check if the board is full
isFull :: Board -> Bool
isFull bd = all (all isJust) bd

-- Check if the game is won by a player
isWonBy :: Board -> Player -> Bool
isWonBy bd p = any (hasFiveConsecutive p) (allLines bd)
  where
    allLines board = rows board ++ cols board ++ diags board
    rows = id
    cols board = map (`column` board) [1..size board]
    diags board = diagonals board -- You need to implement this

    hasFiveConsecutive player line = fiveConsecutive (map (`isMarkedByPlayer` player) line)
    isMarkedByPlayer stone player = stone == Just player

    fiveConsecutive :: [Bool] -> Bool
    fiveConsecutive (a:b:c:d:e:rest) = (a && b && c && d && e) || fiveConsecutive (b:c:d:e:rest)
    fiveConsecutive _ = False

    -- The function 'diagonals'' extracts diagonals in one direction
    diagonals :: Board -> [[Stone]]
    diagonals board = leftDiagonals ++ rightDiagonals
      where
        n = size board
        leftDiagonals = diagonals' id
        rightDiagonals = diagonals' reverse
        -- 'f' is used to reverse the board for the other direction
        diagonals' f = concatMap (\k -> [downRightDiagonal k, downLeftDiagonal k]) [0..2*(n-1)]
          where
            downRightDiagonal k = [board !! i !! j | i <- [0..n-1], j <- [0..n-1], i + j == k]
            downLeftDiagonal k = [board !! i !! j | i <- [0..n-1], j <- [0..n-1], i - j == k - (n-1)]

-- Check if the game is a draw
isDraw :: Board -> Bool
isDraw bd = isFull bd && not (isWonBy bd mkPlayer || isWonBy bd mkOpponent)

-- Check if the game is over
isGameOver :: Board -> Bool
isGameOver bd = isDraw bd || isWonBy bd mkPlayer || isWonBy bd mkOpponent

-- Convert the board to a string for printing
boardToStr :: (Player -> Char) -> Board -> String
boardToStr playerToChar bd =
    unlines [unwords [cellToStr $ bd !! (y - 1) !! (x - 1) | x <- [1..size bd]] | y <- [1..size bd]]
  where
    cellToStr (Just p) = [playerToChar p]
    cellToStr Nothing = "."
