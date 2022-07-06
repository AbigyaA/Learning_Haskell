fact1 :: Int -> Int
fact1 n = if n == 0 then
            1
          else
            n * fact1 (n-1)

fact2 :: Int -> Int
fact2 n
    | n == 0 = 1
    | otherwise = n * fact2 (n-1)

fact3 :: Int -> Int
fact3 0 = 1
fact3 n = n * fact3 (n-1)

fact4 :: Int -> Int
fact4 n
    | n == 0 = 1
    | n >=1 = n * fact4 (n-1)

fact5 :: Int -> Int
fact5 0 = 1
fact5 (n+1) = (n+1) * fact5 n

fact6 :: Int -> Int
fact6 n = product [1..n]
