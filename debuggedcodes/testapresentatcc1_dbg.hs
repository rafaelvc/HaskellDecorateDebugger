module TestesApresentaTCC1 where
import Hugs.Observe
gx n = 2
fx 0 = 1
fx n
  = fx ((gx n) + 1) *
      (fx
         (if (n == 10) then observe "max" (max n 8) else
            observe "min" (min n 7)))