import Observe

gy y = 2 * y

-- fx x = observe "label!" gy x

gx = (+ 1)

fx z = gx z

wx x = 2 + x

zx y = observe "label!" wx (fx y)

fn f = (map f .) . take

fn1 f x = map f . take x

hx x y = x + (y + 3) 

consumer x y =  observe "" (hx x y)
