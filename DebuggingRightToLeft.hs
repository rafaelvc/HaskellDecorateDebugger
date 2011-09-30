import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Pretty


genDebugCode :: FilePath -> IO ()
genDebugCode codesrc = do
	       usercode <- readFile codesrc
	       case parseModule usercode of
		      ParseOk m@(HsModule loc md lexpr limpr ldecl)  -> exploreHsDecl ldecl
		      ParseFailed src ds                              -> putStrLn $ show ds


exploreHsDecl []     = return ( )
exploreHsDecl (x:xs) = 
		 case x of 
			(HsFunBind lstmatch)   -> do {modifiedFunBind lstmatch; exploreHsDecl xs}
			_	                -> exploreHsDecl xs

modifiedFunBind []   = return ( )
modifiedFunBind (m@(HsMatch loc nm pat rhs hsdcl):xs) = 
				case rhs of  
					HsUnGuardedRhs exp -> do {rightSideHsApp exp;modifiedFunBind xs}
					_ 		       -> modifiedFunBind xs

{-
leftSideHsApp HsApp e1 e2 = do {leftSideHsApp e1;rightSideHsApp e2} 
leftSideHsApp HsInfixApp e1 op e2 = do {leftSideHsApp e1;rightSideHsApp e2}
leftSideHsApp HsParen e1 = do leftSideHsApp e1
leftSideHsApp _ = return ( )
different arities 
-}

leftSideHsApp exp = case exp of 
			HsApp e1 e2          -> do {rightSideHsApp e2;leftSideHsApp e1} 
			HsInfixApp e1 op e2   -> do {leftSideHsApp e1;leftSideHsApp e2} 
			HsParen e1           -> leftSideHsApp e1
			_		       -> return ( )

rightSideHsApp exp = case exp of 
			  app@(HsApp e1 e2)    -> do {putStrLn (prettyPrint app); leftSideHsApp app}
			  HsInfixApp e1 op e2   -> do {rightSideHsApp e1;rightSideHsApp e2}
			  HsParen e1           -> rightSideHsApp e1
			  _			 -> return ( )

{- 
instance Show (IO (Maybe a)) where 
	show Nothing = ""

printDebugPoints :: IO ()
printDebugPoints  = do 
			putStrLn "ola"
			return ()	
-}			

