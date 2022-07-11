merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:x1:xs) [] | (x < x1) = [x] ++ merge (x1:xs) []
                   | (x == x1) = [x] ++ merge xs []
                   | otherwise = error ("Lists must be increasing")

merge [] (y:y1:ys) | (y < y1) = [y] ++ merge [] (y1:ys)
                   | (y == y1) = [y] ++ merge [] ys
                   | otherwise = error ("Lists must be increasing")

merge (x:[]) (y:y1:ys) | ((x < y) && (y < y1)) = [x,y] ++ merge [] (y1:ys)
                       | ((x > y) && (y < y1)) = [y] ++ merge (x:[]) (y1:ys)
                       | ((x == y) && (y < y1)) = [x] ++ merge [] (y1:ys)
                       | ((x < y) && (y == y1)) = [x,y] ++ merge [] (ys)
                       | ((x > y) && (y == y1)) = [y] ++ merge (x:[]) (ys)
                       | ((x == y) && (y == y1)) = [x] ++ merge [] (ys)
                       | otherwise = error ("Lists must be increasing")

merge (x:x1:xs) (y:[]) | ((x < y) && (x < x1)) = [x] ++ merge (x1:xs) (y:[])
                       | ((x > y) && (x < x1)) = [y,x] ++ merge (x1:xs) []
                       | ((x == y) && (x < x1)) = [x] ++ merge (x1:xs) []
                       | ((x < y) && (x == x1)) = [x] ++ merge (xs) (y:[])
                       | ((x > y) && (x == x1)) = [y,x] ++ merge (xs) []
                       | ((x == y) && (x == x1)) = [x] ++ merge (xs) []
                       | otherwise = error ("Lists must be increasing")

merge (x:[]) (y:[]) | (x < y) = [x,y] 
                    | (x == y) = [x] 
                    | (x > y) = [y,x]

merge [] (y:[]) = [y]
merge (x:[]) [] = [x]

merge (x:x1:xs) (y:y1:ys) | ((x < x1) && (y < x) && (y <= y1)) = [y] ++ merge (x:x1:xs) (y1:ys)
                          | ((x < x1) && (y > x) && (y < y1)) = [x] ++ merge (x1:xs) (y:y1:ys)
                          | ((x < x1) && (y >= x) && (y <= y1)) = [x] ++ merge (x1:xs) (y1:ys)
                          | ((x < x1) && (y == x) && (y == y1)) = [x] ++ merge (x1:xs) (ys)
                          | ((x == x1) && (y < x) && (y < y1)) = [y] ++ merge (x1:xs) (y1:ys)
                          | ((x == x1) && (y > x) && (y < y1)) = [x] ++ merge (xs) (y:y1:ys)
                          | ((x == x1) && (y >= x) && (y <= y1)) = [x] ++ merge (xs) (y1:ys)
                          | ((x == x1) && (y < x) && (y == y1)) = [y] ++ merge (x1:xs) (ys)
                          | ((x == x1) && (y == x) && (y == y1)) = [x] ++ merge (xs) (ys)
                          | otherwise = error ("Lists must be increasing")




