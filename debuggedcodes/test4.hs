
import Observe
import Debug.Trace 

main = runO example

gy y = 2 * y

fx x = observe "gy" (gy x)

example :: IO () 
example = print
         ((observe "length" :: Observing ([Int] -> Int))
        	length [1..3]
    	 )

-- observe :: (Observable a) => String -> a -> a

{- 
	fat :: Int -> Int
	fat 0 = 1
	fat n = n * (observe  "fat" (fat (n-1)))
-} 


-- trace :: String -> a -> a

fat 0 = 1
fat n = n * (trace ("fat " ++ (show n)) (fat (n-1)))



-- x n = runO (fat n)





{-

wx x = 2 + x
zx y = observe "label!" wx (observe "observando fx em wx" fx y)
fn f = (map f .) . take
fn1 f x = map f . take x

-}
