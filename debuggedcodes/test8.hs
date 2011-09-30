
gx x y = x + y
fx x y z = x + y + z 

hx p0 p1 p2 = hx (fx p0 p1 p2) (gx p1 p2) (gx p0 (gx p0 p1))

