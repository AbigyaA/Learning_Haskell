map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs 

squareAll :: [Int] -> [Int]
squareAll xs = map' sq xs 
               where sq x = x * x 

lengthAll :: [[a]] -> [Int]
lengthAll xss = map' length xss 

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) | p x = x : xs'
                 | otherwise = xs'
                             where xs' = filter' p xs 

getEven2 :: [Int] -> [Int]
getEven2 xs = filter' even xs 

doublePos2 :: [Int] -> [Int]
doublePos2 xs = map' db1 (filter' pos xs)
                where db1 x = 2 * x 
                      pos x = (0 < x)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss 

sumlist :: [Int] -> Int 
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs 

foldrx :: (a -> b -> b) -> b -> [a] -> b 
foldrx f z [] = z
foldrx f z (x:xs) = f x (foldrx f z xs)

concat2 :: [[a]] -> [a]
concat2 xss = foldrx (++) [] xss 

sumlist2 :: [Int] -> Int 
sumlist2 xs = foldrx (+) 0 xs 

and', or' :: [Bool] -> Bool 
and' xs = foldrx (&&) True xs 
or' xs = foldrx (||) False xs 

foldlx :: (a -> b -> a) -> a -> [b] -> a 
foldlx f z [] = z
foldlx f z (x:xs) = foldlx f (f z x) xs 

doublePos3 :: [Int] -> [Int]
doublePos3 xs = map' ((*) 2) (filter' ((<) 0) xs) 

flip' :: (a -> b -> c) -> b -> a -> c 
flip' f x y = f y x 

sumCubes :: [Int] -> Int 
sumCubes xs = sum (map' (^3) xs)

count :: Int -> [[a]] -> Int 
count n | n >= 0 = length . filter' (== n) . map' length
        | otherwise = const 0

any', all' :: (a -> Bool) -> [a] -> Bool
any' p = or' . map' p 
all' p = and' . map' p 

length' :: [a] -> Int
length' = foldl (\n _ -> n+1) 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p xs@(x:xs') | p x = dropWhile' p xs'
                        | otherwise = xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipWith' z (x:xs) (y:ys) = z x y : zipWith' z xs ys
zipWith' _ _ _ = []
