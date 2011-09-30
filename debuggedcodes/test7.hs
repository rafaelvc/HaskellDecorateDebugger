-- import Hugs.Observe

gx n = 2
fat 0 = 1
fat n = fat ( (gx n) + 1) * fat (n-1)


fat n = observe "fat" (fat ((observe "gx" (gx n)) + 1)) * observe "fat" (fat (n - 1))



g x   = 1
h x   = 2

-- f x y = g (h (h x))
-- f x y = observe "g" (g (observe "h" (h (observe "h" (h x)))))

sh x y = g (h (y+1) (h x))
-- h x y = observe "g" (g (observe "h" (h (y + 1) (observe "h" (h x)))))

{- 
h 0 = 1
h x = observe ("h " ++ (show x)) (h (x-1)) + 1
g x = observe "g " h (x + 1)
-}

{-
fx x y = (g " (g x) ) + ( observe "h " (h y))
fat 0 = 1
fat n = fat ( fat (n-1) ) 
-}

{-
subseqSublist l1 l2 = 
            let
                subl = sublist l1 l2
                subs = subsequence l1 l2
            in (
                if subs then
                    putStr (l1 ++ " is a SUBLIST and a SUBSEQUENCE of "  ++ l2)
                else
                    if subl then
                        putStr (l1 ++ " is only SUBLIST of " ++ l2)
                    else
                        putStr (l1 ++ " isn't SUBLIST nor SUBSEQUENCE of " ++ l2)
               )
-}












