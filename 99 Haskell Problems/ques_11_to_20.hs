-- Problem 11
data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

