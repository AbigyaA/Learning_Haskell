mult :: Int -> Int -> Int 
mult a b | ((a < 0) || (b < 0)) = error ("Numbers have to be greater than zero")
         | ((a == 0) || (b == 0)) = 0
         | otherwise = b + mult (a - 1) b 