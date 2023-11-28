isHamming :: Integer -> Bool
-- Base case: 1 is a Hamming number
isHamming 1 = True
isHamming n
    -- If n is divisible by 2, divide it by 2 and recursively check if the result is a Hamming number
    | n `mod` 2 == 0 = isHamming (n `div` 2)
    -- If n is divisible by 3, divide it by 3 and recursively check if the result is a Hamming number
    | n `mod` 3 == 0 = isHamming (n `div` 3)
    -- If n is divisible by 5, divide it by 5 and recursively check if the result is a Hamming number
    | n `mod` 5 == 0 = isHamming (n `div` 5)
    -- If n is not divisible by 2, 3, or 5, it is not a Hamming number
    | otherwise = False
