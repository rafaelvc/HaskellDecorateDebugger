

-- import Hugs.Observe 
gy y = return y + 1

zx :: Int -> IO ()
zx y = do {
		x <- gy (gy 20);
		z <- gy 10;
		x <- if z > 10 then gy (gy (gy 50)) else gy 60;
		putStr (show x);
	}


zh = let gg x = 40
	 in (gg 0)
