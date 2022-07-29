{-# LANGUAGE ViewPatterns #-}
-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n | n <= 0 = []
           | otherwise = toDigits (n `div` 10) ++ [(n `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = []
              | otherwise = [(n `mod` 10)] ++ toDigitsRev (n `div` 10)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (reverse -> (x:y:zs)) = doubleEveryOther (reverse zs) ++ [2*y, x]
doubleEveryOther _ = []

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits lst = sum $ map (sum . toDigits) lst