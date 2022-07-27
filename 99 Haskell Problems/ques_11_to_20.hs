-- Problem 11
data ListItem a = Single a | Multiple Int a
    deriving (Show)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode ls  =  encode' (pack ls)
          where encode' (x:xs) =  (length x, head x) : encode' xs
                encode' [] = []

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

