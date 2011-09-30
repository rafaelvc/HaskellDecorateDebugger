module Testtcc where 


{- 
gx n = 2
fat 0 = 1
fat n = fat((gx n) + 1) * (fat(n-1))
-} 


fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo(n-2)

