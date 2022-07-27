-- Problem 1
myLast :: [a] -> a 
myLast [] = error "Empty List"
myLast (x:[]) = x
myLast (x:xs) = myLast xs 

-- Problem 2
myButLast :: [a] -> a 
myButLast [] = error "Empty List"
myButLast (x:x1:[]) = x 
myButLast (x:x1:xs) = myButLast (x1:xs) 

-- Problem 3
elementAt :: [a] -> Int -> a 
elementAt [] _ = error "Empty List"
elementAt (x:xs) n | n == 1 = x 
                   | n <= length (x:xs) = elementAt xs (n-1)
                   | otherwise = error "Number is greater than length of list"

-- Problem 4
myLength :: [a] -> Int 
myLength [] = 0
myLength (x:xs) = 1 + myLength xs 

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs | xs == myReverse xs = True
                | otherwise = False

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a] 
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:ys@(y:_)) | x == y = compress ys 
                      | otherwise = x : compress ys 
compress ys = ys    

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode ls  =  encode' (pack ls)
          where encode' (x:xs) =  (length x, head x) : encode' xs
                encode' [] = []