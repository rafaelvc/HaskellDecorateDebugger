

import Observe


main = runO example
example :: IO () 
example = print( observe "length"  length [1..3] )


{-
import Observe 


debug = runO 


fat 0 = 1
fat n = n * (observe "fat " (fat (n-1))

-}
