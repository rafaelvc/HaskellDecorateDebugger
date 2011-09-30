-- import Observe 

gy y = 2 * y

fx x = gy x

wx x = 2 + x
 
-- zx :: Int -> Int 
zx y = wx ( observe "observando fx em wx" fx y )

fn f = (map f . ) . take 

fn1 f x = map f . take x

{- 
putStrO :: (Show a) => a -> IO ()
putStrO expr = run0 (putStr expr)

-}

{- Observe from Hugs 

import Hugs.Observe

prog n = do { setBkpt "fib" True; putStr $ show (observe "fun" f n) }
f 0 = 1
f n = n * (bkpt "fib" $ observe "fun" f (n-1))

-}

-- fn2 f n l = map f (take n l) -- point free 

-- funcaoqq n = map (+ 1) (observe "list de 1 a 20" take n [1 .. 20])



{-

-----------------------------------------------------------

main = runO ex8

ex8 :: IO
ex8 = print
    ((observe "length" :: Observing ([Int] -> Int))
        length [1..3]
    )

-- length
  { \ (_ : _ : _ : []) -> 3
  }



----------------------------------------------------------

main = runO ex9

ex9 :: IO ()
ex9 = print
      ((observe "foldl (+) 0 [1..4]" 
               :: Observing ((Int -> Int -> Int) -> Int -> [Int] -> Int)
       ) foldl (+) 0 [1..4]
      )


-- foldl (+) 0 [1..4]
  { \ { \ 6 4 -> 10
      , \ 3 3 -> 6
      , \ 1 2 -> 3
      , \ 0 1 -> 1
      } 
      0
      ( 1 : 2 : 3 : 4 : [])
      -> 10
  }


-----------------------------------------------------------


main = runO (print (ex8 3408))

ex10 :: Int -> [Int]
natural
 = observe "reverse"             reverse 
 . observe "map (`mod` 10)"      map (`mod` 10)
 . observe "takeWhile (/= 0)"    takeWhile (/= 0)
 . observe "iterate (`div` 10)"  iterate (`div` 10)


-- iterate (`div` 10)
  { \ { \ 3  -> 0
      , \ 34  -> 3
      , \ 340  -> 34
      , \ 3408  -> 340
      } 3408
       -> 3408 : 340 : 34 : 3 : 0 : _
  }
-- map (`mod` 10)
  { \ { \ 3  -> 3
      , \ 34  -> 4
      , \ 340  -> 0
      , \ 3408  -> 8
      } (3408 : 340 : 34 : 3 : [])
       -> 8 : 0 : 4 : 3 : []
  }
-- reverse
  { \ (8 : 0 : 4 : 3 : [])  -> 3 : 4 : 0 : 8 : []
  }
-- takeWhile (/= 0)
  { \ { \ 0  -> False
      , \ 3  -> True
      , \ 34  -> True
      , \ 340  -> True
      , \ 3408  -> True
      } (3408 : 340 : 34 : 3 : 0 : _)
       -> 3408 : 340 : 34 : 3 : []
  }

-}


