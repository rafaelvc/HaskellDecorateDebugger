module Tguardas where
import Hugs.Observe
teste x
  | (x == 1) = 1
  | (x == 2) = 2
  | (x == 3) = 3
  | otherwise = observe "teste2" (teste2 (teste 2))
teste2 x = gx $ gy $ gy x
gx x = 1
gy y = 1
zx y = do putStr ("aa" ++ y)