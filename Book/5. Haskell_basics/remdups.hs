remdups :: [Int] -> [Int]
remdups (x:y:xs) 
    | x == y = remdups (y:xs)
    | x /= y = x : remdups (y:xs)
remdups xs = xs     