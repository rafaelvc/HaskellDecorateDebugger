module TestesApresentaTCC where
import Hugs.Observe
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + observe "fibo" (fibo (n - 2))
fat 0 = 1
fat n = n * observe "fat" (fat (n - 1))