
module TestesApresentaTCC1 where 


hx n = return 2

{-
gx n = 2

fx 0 = 1
fx n = fx((gx n) + 1) * (fx( if (n == 10) then max n 8 else min n 7 ))
fx n | (n == 2) = 10
fx n | (n == 1) = gx n
-}

{- fx n = do {
	    x <- (hx (hx 2)) + (fx 3);
		x <- 10;
	    if  x > 10 then 
	    	return n
        else 
        	return 0;
} -}

--clausulas where!
{-
gx n = y
	where 
		y = hx n
-}

-- clausulas case
{-
gx n = case n of 
		 _ -> (gx 0)
-}


gy n = True
hy n = True

-- clausulas Let 
gx n = let  --Declaracoes
			tx :: Int -> Int 
		 	tx y = y + 1
		in (tx n)

-- This is path bind !
c@(a, b, d) = (1, [1 .. gx 2], [(a + (gx a))  | gy a, hy a])


d n = []


j d@(a, b, d) = (gx a, [1 .. gx 2], [(a + (gx a))  | gy a, hy a])













