mean :: [Int] -> Int 
mean [] = 0
mean xs = (sum xs) `div` (length xs)