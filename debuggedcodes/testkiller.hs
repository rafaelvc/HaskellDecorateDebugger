    	

-- $Id: Shrink.hs,v 1.3 2006/03/27 13:09:49 rahn Exp $

module Leader.Shrink where

import Leader.CA
import Leader.Direction
import Leader.Pattern
import Leader.Space
import Leader.Verify

import Util.Fixpoint
import Util.PPM

import Data.List
import Control.Monad

-------------------------------------------------------------------------------

type ShrinkSet = [Direction]

default_shrink_set :: Dimension -> ShrinkSet
default_shrink_set = dirs

-------------------------------------------------------------------------------

type Phase = Int

phase0 :: Phase
phase0 = 0

next :: ShrinkSet -> Phase -> Phase
next s p = mod ( succ p ) ( 2 * ( length s ) )

is_update :: Phase -> Bool
is_update = odd

now_dir :: Phase -> ShrinkSet -> Direction
now_dir p s = s !! ( div p 2 )

-------------------------------------------------------------------------------

type Piece = [Direction]

data State = Pattern
	   | Leader
	   | Done
	   | Pre_Done
	   | Q Phase Piece
	     deriving ( Eq , Ord , Show )

alive :: State -> Bool
alive Done     = False
alive Pre_Done = False
alive _        = True

the_piece :: State -> Piece
the_piece ( Q _ ps ) = ps
the_piece _          = []

piece :: Phase -> [(State,Direction)] -> State
piece p qs = case map snd $ filter ( alive . fst ) qs of
	      []  -> Leader
	      [_] -> Pre_Done
	      ds  -> Q p ds

-------------------------------------------------------------------------------

is_border_new :: Direction -> Piece -> [(Piece,Direction)] -> Bool
is_border_new r ps qs = and [ not $ elem           r  ps
			    ,       elem (opposite r) ps
			    , not $ null os
			    , and $ do
			      (p,s) <- qs
			      guard $ elem s os
			      return $ and [       elem (opposite r) p
					   , not $ elem           r  p
					   ]
			    ]
    where ns = map snd qs
	  os = do s <- ns
		  guard $ is_orthogonal r s
		  return s

is_border :: Direction -> Piece -> [(Piece,Direction)] -> Bool
is_border r ps qs = and [ not $ elem           r  ps
			,       elem (opposite r) ps
			, not $ null os
			, and $ do
			  (p,s) <- qs
			  guard $ elem s os
			  return $ elem (opposite r) p
			]
    where ns = map snd qs
	  os = do s <- ns
		  guard $ is_orthogonal r s
		  return s

-------------------------------------------------------------------------------

shrink_ca :: Dimension -> ShrinkSet -> CA State
shrink_ca _ _ Leader _                       = Leader
shrink_ca _ _ Done   _                       = Done

shrink_ca _ _ Pre_Done qs
    | and [ length ps == 1
	  , is_positive $ head ps
	  ]                                  = Leader
    where ps = do (q,r) <- qs
		  guard $ q == Pre_Done
		  return r
shrink_ca _ _ Pre_Done _                     = Done

shrink_ca _ _ _ qs
    | null alives                            = Leader
    | length alives == 1                     = Pre_Done
    where alives = filter ( alive . fst ) qs

shrink_ca _ _ Pattern qs                     = piece phase0 qs

shrink_ca _ s (Q p _) qs
    | is_update p                            = piece (next s p) qs

shrink_ca _ s (Q p _) qs
    | and [ alive q'
	  , is_border (now_dir p s) ds' ps
	  ]                                  = Done
    | otherwise                              = q'
    where q'        = piece (next s p) qs
	  (Q _ ds') = q'
	  ps        = do (q,r) <- qs
			 guard $ alive q
			 return (the_piece q,r)

-------------------------------------------------------------------------------

shrink_cs :: Dimension -> Pattern -> ShrinkSet -> [ Config State ]
shrink_cs d pat s = fixLN ( 2 * ( length s ) )
		  $ calc ( shrink_ca d s ) $ conf_const d pat Pattern

-------------------------------------------------------------------------------

instance PPM_Pixel ( Maybe State ) where
    ppm_pixel Nothing             = frame $ full white
    ppm_pixel ( Just Pattern    ) = frame $ full black
    ppm_pixel ( Just Leader     ) = leader
    ppm_pixel ( Just Done       ) = done
    ppm_pixel ( Just Pre_Done   ) = pre_done
    ppm_pixel ( Just ( Q p ds ) )
	| is_update p             = ppm_pixel $ Just ( ( Left ( map opposite ds ) , 0 ) :: TT )
	| otherwise               = ppm_pixel $ Just ( ( Left ( map opposite ds ) , 1 ) :: TT )

-------------------------------------------------------------------------------

verify :: Dimension -> Pattern -> ( Int , Bool )
verify d pat = gen_verify ( == Leader ) ( == Done ) $ shrink_cs d pat
	                                            $ default_shrink_set d

ppms :: FilePath -> Pattern -> IO [()]
ppms base pat = gen_ppms base $ shrink_cs 2 pat
		              $ default_shrink_set 2

