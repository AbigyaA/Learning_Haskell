-- maxValue :: (Ord a) => [a] -> a 
maxValue :: [Int] -> Int 

maxValue (x:y:[]) | x < y = y
                  | x >= y = x 

maxValue (x:y:xs) | x < y = maxValue (y:xs)
                  | x > y = maxValue (x:xs)