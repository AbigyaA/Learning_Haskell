rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

rev' [] ys = ys
rev' (x:xs) ys = rev' xs (x:ys)

reverse' :: [a] -> [a]
reverse' xs = rev xs []
              where rev [] ys = ys
                rev (x:xs) ys = rev xs ++ (x:ys)
