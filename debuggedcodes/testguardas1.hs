







f = foldl (\x y -> counter x y) 0 [1,2,3]
counter x y = return (x + y)
