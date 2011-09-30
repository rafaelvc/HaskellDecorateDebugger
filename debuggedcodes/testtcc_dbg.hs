module Testtcc where
import Hugs.Observe
 
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n
  = observe "fibo" (fibo (n - 1)) + observe "fibo" (fibo (n - 2))