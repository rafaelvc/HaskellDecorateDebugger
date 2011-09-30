import Observe

fat 0 = 1
fat n = n * ((observe "fat" :: Observing (Int-> Int)) fat (n-1))

{-
fatDebug :: IO()
fatDebug = print (fat 4)
main = runO fatDebug 	
-}
fatDebug = runO (print (fat 4))
