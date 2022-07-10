-- a
hailstone :: Int -> Int 
hailstone 1 = 1
hailstone n | ((n > 1) && ((n `mod` 2) == 0)) = hailstone (n `div` 2)
            | ((n > 1) && ((n `mod` 2) /= 0)) = hailstone ((3 * n) + 1)

-- b
-- hailstone :: Int -> Int 
-- hailstone 1 = 1

-- hailstone n | ((n > 1) && ((n `mod` 2) == 0)) = (n `div` 2)
--             | ((n > 1) && ((n `mod` 2) /= 0)) = ((3 * n) + 1)

-- hailstone n | ((n > 1) && ((n `mod` 2) == 0)) = hailstone (n `div` 2)
--             | ((n > 1) && ((n `mod` 2) /= 0)) = hailstone ((3 * n) + 1)