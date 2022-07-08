fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n | n > 1 = fib (n-2) + fib (n-1)

fib' :: Int -> Int 
fib' n = fib'' n 0 1
         where fib'' 0 p q = p
               fib'' n p q | n > 0 = fib'' (n-1) q (p+q)
