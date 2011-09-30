
module DebugLocalizationCode where

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import Control.Monad


showDebugPoints :: FilePath -> IO ()
showDebugPoints codesrc = do
	       usercode <- readFile codesrc
	       case parseModule usercode of
		      ParseOk m@(HsModule loc md lexpr limpr ldecl) -> do {  
				putStrLn "\nDebug points:\n "; 
				exploreHsDecl 0 ldecl } 
		      ParseFailed src ds               -> putStrLn $ show ds


exploreHsDecl cont [] = return ()
exploreHsDecl cont (x:xs)  =  
              case x of 
               (HsFunBind lstmatch) -> do {
				contl <- exploreHsFunBind cont lstmatch;
				exploreHsDecl contl xs}
               _	            -> exploreHsDecl cont xs

exploreHsFunBind cont []   = return cont
exploreHsFunBind cont (m@(HsMatch loc nm pat rhs hsdcl):xs) = 
		case rhs of  
		  HsUnGuardedRhs exp -> do {
				contl <- rightSideHsApp cont exp; 
				exploreHsFunBind contl xs}
		  HsGuardedRhss lgrd -> do { 
				contl <- 
				foldM ( \x (HsGuardedRhs loc e1 e2) -> 
                                rightSideHsApp x e2 ) cont lgrd;
				exploreHsFunBind contl xs; }
                  _ 		     -> exploreHsFunBind cont xs

exploreHsDo cont  (HsGenerator loc pat e) = rightSideHsApp cont e
exploreHsDo cont  (HsQualifier e)         = rightSideHsApp cont e
exploreHsDo cont _   = return cont 


leftSideHsApp cont exp = case exp of 
			HsApp e1 e2          -> do {
				contl <- leftSideHsApp cont e1;
				rightSideHsApp contl e2} 
			HsInfixApp e1 op e2  -> do {
				contl <- leftSideHsApp cont e1;
				leftSideHsApp contl e2} 
			HsDo lstmt 	     -> do {
					contl <- foldM (exploreHsDo) cont lstmt;
					return contl; }
			HsList e1s  -> do {
					contl <- foldM (leftSideHsApp) cont e1s;
					return contl; }
			HsTuple e1s -> do {
					contl <- foldM (leftSideHsApp) cont e1s;
					return contl; }
		  	HsIf e1 e2 e3        -> do {
				contl <- leftSideHsApp cont e2;
				leftSideHsApp contl e3 }
			HsParen e1           -> leftSideHsApp cont e1
			_		     -> return cont

rightSideHsApp cont exp = case exp of 
			  HsApp e1 e2    -> do {
				putStrLn ((show cont)  ++ " - " ++ (prettyPrint exp));
				leftSideHsApp (cont+1) exp}
			  HsInfixApp e1 op e2  -> do {
				contl <- rightSideHsApp cont e1;
				rightSideHsApp contl e2}
			  HsDo lstmt 	       -> do {
					contl <- foldM (exploreHsDo) cont lstmt;
					return contl; }
			  HsList e1s  -> do {
					contl <- foldM (rightSideHsApp) cont e1s;
					return contl; }
			  HsTuple e1s -> do {
					contl <- foldM (rightSideHsApp) cont e1s;
					return contl; }
			  HsIf e1 e2 e3        -> do {
				contl <- rightSideHsApp cont e2;
				rightSideHsApp contl e3 }
			  HsParen e1           -> rightSideHsApp cont e1
			  _		       -> return cont


extractFuncName str = let fname = show str
			     in ( if (take 7 fname) == "HsIdent" then 
					drop 7 fname 
				  else "nonamefunc")


fshowsintax :: FilePath -> IO ()
fshowsintax f = 
		do
	             c <- readFile f
		     case parseModule c of
		     	ParseOk m           -> putStrLn $ show m
		     	ParseFailed src ds  -> putStrLn $ show ds


