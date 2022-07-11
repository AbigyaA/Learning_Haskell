-- a
natToBin :: Int -> [Int]
natToBin 1 = [1]
natToBin n | (n > 1) = ((natToBin (n `div` 2)) ++ [(n `rem` 2)])

-- b
natToBase :: Int -> Int -> [Int]
natToBase _ 1 = [1]
natToBase b n | ((b >= 2) && (n > 1)) = ((natToBase b (n `div` b) ++ [(n `rem` b)]))

-- c
baseToNat :: Int -> [Int] -> Int
baseToNat b [] = 0
baseToNat b (x:xs) | (b >= 2) = (x * (b^(length xs))) + baseToNat b xs