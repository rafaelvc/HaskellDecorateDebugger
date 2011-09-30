

{-
import Observe

fat 0 = 1
fat n = n * fat (n­ - 1)

fatDebug :: IO () 
fatDebug = print
         ((observe "fat" 
	   :: Observing (Int ­-> Int)
	  )fat 5
         )

main = runO fatDebug
-}

import Observe
main = runO example
example :: IO () 
example = print
         ((observe "length" :: Observing ([Int] ­-> Int))
         length [1..3]
      )

