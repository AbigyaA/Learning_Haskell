f :: [Int] -> [Int]
f xs = let square a = a*a
           one = 1
           (y:ys) = xs
        in (square y + one) : f ys 

g :: Int -> Int
g n | (n `mod` 3) == x = x
    | (n `mod` 3) == y = y
    | (n `mod` 3) == z = z
                    where x = 0
                          y = 1
                          z = 2
