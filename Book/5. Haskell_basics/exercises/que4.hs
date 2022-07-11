adjpairs :: [a] -> [(a,a)]
adjpairs (x:y:[]) = (x,y)
adjpairs (x:y:xs) = (x,y) : adjpairs (y:xs)