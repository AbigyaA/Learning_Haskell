module Golf where 

-- Exercise 1
-- skips :: [a] -> [[a]]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima (x:y:[]) = []
localMaxima (x:y:xs@(z:_)) | y > x && y > z = [y] ++ localMaxima (y:xs)
                           | otherwise = localMaxima (y:xs)