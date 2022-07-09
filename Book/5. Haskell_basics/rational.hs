type Rat = (Int, Int)

normRat :: Rat -> Rat 
normRat (x,0) = error ("Invalid rational number (in normRat) " ++ showRat (x,0) ++ "\n")
normRat (0,y) = (0,1)
normRat (x,y) = (a `div` d, b `div` d)
                where a = (signum' y) * x 
                      b = abs y
                      d = gcd' a b

signum' :: (Num a, Ord a) => a -> Int 
signum' n | n == 0 = 0
          | n > 0 = 1
          | n < 0 = -1 

gcd' :: Int -> Int -> Int 
gcd' x y = gcd'' (abs x) (abs y)
          where gcd'' x 0 = x 
                gcd'' x y = gcd'' y (x `rem` y)

negRat :: Rat -> Rat 
negRat (a,b) = normRat (-a,b)

addRat, subRat, mulRat, divRat :: Rat -> Rat -> Rat
addRat (a,b) (c,d) = normRat (a*d + c*b, b*d)
subRat (a,b) (c,d) = normRat (a*d - c*b, b*d)
mulRat (a,b) (c,d) = normRat (a*c, b*d)
divRat (a,b) (c,d) = normRat (a*d, b*c)

eqRat :: Rat -> Rat -> Bool
eqRat (a,0) (c,d) = errRat (a,0)
eqRat (a,b) (c,0) = errRat (c,0)
eqRat (a,b) (c,d) = (a*d == b*c)

ltRat :: Rat -> Rat -> Bool
ltRat (a,0) (c,d) = errRat (a,0)
ltRat (a,b) (c,0) = errRat (c,0)
ltRat (a,b) (c,d) = (a*d < b*c)

gtRat :: Rat -> Rat -> Bool
gtRat (a,0) (c,d) = errRat (a,0)
gtRat (a,b) (c,0) = errRat (c,0)
gtRat (a,b) (c,d) = (a*d > b*c)

errRat (x,y) = error ("Invalid rational number" ++ showRat (x,y))

showRat :: Rat -> String
showRat (a,b) = show a ++ "/" ++ show b