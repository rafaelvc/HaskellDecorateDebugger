import Language.Haskell.Syntax

abc 0 _ _ = 0
abc x y c =  abc x y c

fat 0 = 
-- fat n = n * fat (n-1) 
fat n = observe "fat" (fat (n-1))



