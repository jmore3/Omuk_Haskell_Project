-- Hides take function from Prelude so that we can use our own take function

import Prelude hiding (take)

take :: Int -> [a] -> [a]

-- If n is 0, return an empty list

take 0 _ = []

-- If xs is empty, return an empty list

take _ [] = []

-- Otherwise, take the head element `x` and prepend it to the result of
-- recursively calling `take` on the remaining elements of `xs` with `n-1`

take n (x:xs) = x : take (n-1) xs