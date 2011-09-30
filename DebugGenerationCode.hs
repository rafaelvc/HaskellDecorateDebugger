

module DebugGenerationCode where

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Pretty


--genDebugCode :: FilePath -> [Int] -> IO ()
genDebugCode codesrc debugpts (lib,func) = do
		  usercode <- readFile codesrc
		  case parseModule usercode of
		      ParseOk m@(HsModule loc md lexpr limpr ldecl)  -> 
			let newsrcdbg = prettyPrint $ HsModule loc md lexpr (addImportObserve limpr "Hugs.Observe") (exploreHsDecl 0 ldecl debugpts "observe")
			in (writeFile (genFileDbgName codesrc) newsrcdbg)
		      ParseFailed src ds              -> putStrLn $ (show ds)

exploreHsDecl cont [] _ _   = [] 
exploreHsDecl cont (l:ls) debugpts func = 
		 case l of 
			HsFunBind lmatch     -> 
					let (newlmatch, newcont, newdebgpts) = exploreHsFunBind lmatch cont debugpts func
					in ( if null newlmatch then []
					     else (HsFunBind newlmatch) : (exploreHsDecl newcont ls newdebgpts func) )						
			_	             -> l : exploreHsDecl cont ls debugpts func

-- exploreHsFunBind :: [HsMatch] -> Int -> [Int] -> ([HsMatch], Int, [Int])
exploreHsFunBind [] cont debugpts _ =  ([], cont, debugpts)
exploreHsFunBind (match@(HsMatch loc nm pat rhs hsdcl):ls) cont debugpts func = 
				case rhs of  
					HsUnGuardedRhs exp -> 
						let (newexp, newcont, newdebgpts) = rightSideHsApp exp cont debugpts func
						    newmatch = HsMatch loc nm pat (HsUnGuardedRhs newexp) hsdcl
						    (matchs, conts, debugptss) = exploreHsFunBind ls newcont newdebgpts func
 						 in ((newmatch:matchs), conts, debugptss)
					HsGuardedRhss exp -> 
						let (newexp, newcont, newdebgpts) = exploreHsGuarded exp cont debugpts func
						    newmatch = HsMatch loc nm pat (HsGuardedRhss newexp) hsdcl
						    (matchs, conts, debugptss) = exploreHsFunBind ls newcont newdebgpts func
 						in  ((newmatch:matchs), conts, debugptss)
					_ 		   -> 
						let (matchs, conts, debugptss) = exploreHsFunBind ls cont debugpts func
						in  ((match:matchs), conts, debugptss)

exploreHsGuarded [] cont debugpts _ = ([], cont, debugpts)
exploreHsGuarded ((HsGuardedRhs loc e1 e2):ls) cont debugpts func = 
				let
				    (newe2, newcont, newdebgpts) = rightSideHsApp e2 cont debugpts func									
				    newguard = HsGuardedRhs loc e1 newe2
				    (guards, conts, debugptss) = exploreHsGuarded ls newcont newdebgpts func
				in  ((newguard:guards), conts, debugptss)

-- Realizar Observe de Apliacacao de Funcao nos seus parametros
leftSideHsApp exp cont [] _ = (exp, cont, [])
leftSideHsApp exp cont debgpt func = case exp of 
			HsApp e1 e2          -> 
			  let (newe1, newcont1, newdebgpt1) = leftSideHsApp e1 cont debgpt func
			      (newe2, newcont2, newdebgpt2) = rightSideHsApp e2 newcont1 newdebgpt1 func
			  in (HsApp newe1 newe2, newcont2, newdebgpt2 )
			HsInfixApp e1 op e2  -> 
			  let (newe1, newcont1, newdebgpt1) = leftSideHsApp e1 cont debgpt func
			      (newe2, newcont2, newdebgpt2) = leftSideHsApp e2 newcont1 newdebgpt1 func
			  in (HsInfixApp newe1 op newe2, newcont2, newdebgpt2)
			HsList el  -> 

			HsList newList
				where 
					newList ex cont [] = (ex, cont, [])
					newList (x:xs) = let (nexp, ncont, ndebugpnts) = rightSideHsApp exp cont x

									 in (nexp:(rightSideHsApp exp ncont xs)

					
-> []

				map (rightSideHsApp exp cont) e1s

				 e1s
				\ (exp, cont, debgpt) -> 
				in (HsList map 
			HsTuple e1s -> do {
					contl <- foldM (leftSideHsApp) cont e1s;
					return contl; }
			HsDo lstmt 	     -> 
				let (newe1, newcont1, newdebgpt1) = exploreHsDo lstmt cont debgpt func
				in (HsDo newe1, newcont1, newdebgpt1) 
			HsIf      e1 e2 e3   -> 
			  let (newe2, newcont1, newdebgpt1) = leftSideHsApp e2 cont debgpt func
			      (newe3, newcont2, newdebgpt2) = leftSideHsApp e3 newcont1 newdebgpt1 func
			  in (HsIf e1 newe2 newe3, newcont2, newdebgpt2)
			HsParen e1           -> 
			  let (newe1, newcont1, newdebgpt1) = leftSideHsApp e1 cont debgpt func
			  in (HsParen newe1, newcont1, newdebgpt1)
			_		     -> (exp, cont, debgpt) 

exploreHsDo (l:ls) cont debugpts _ = ([], cont, debugpts)
exploreHsDo (l:ls) cont debugpts func = case l of
		HsGenerator loc pat exp -> let (newexp, newcont, newdebgpt) = rightSideHsApp exp cont debugpts func
                                               newHsGenerator = HsGenerator loc pat newexp
                                               (exps, conts, debugptss) = exploreHsDo ls newcont newdebgpt func
					     in ((newHsGenerator:exps), conts, debugptss)
		HsQualifier exp 	-> let (newexp, newcont, newdebgpt) = rightSideHsApp exp cont debugpts func
                                               newHsQualifier = HsQualifier newexp
                                               (exps, conts, debugptss) = exploreHsDo ls newcont newdebgpt func
					     in ((newHsQualifier:exps), conts, debugptss)
		_			-> let (exps, conts, debugptss) = exploreHsDo ls cont debugpts func
					     in ((l:exps), conts, debugptss)

rightSideHsApp exp cont [] _ = (exp, cont, []) 
rightSideHsApp exp cont debgpt func = case exp of 
		        app@(HsApp e1 e2)    -> 
				if any ( == cont)  debgpt then 
			          let newDbgpt = filter (/= cont) debgpt
				      fname = functionNameHsApp app
				      (newe1, newcont1, newdebgpt1) = leftSideHsApp app (cont + 1) newDbgpt func
				  in 
				   (newHsApp fname newe1 func, newcont1, newdebgpt1)
				else
				  leftSideHsApp app (cont + 1) debgpt func
			HsInfixApp e1 op e2  -> 
				let (newe1, newcont1, newdebgpt1) = rightSideHsApp e1 cont debgpt func
				    (newe2, newcont2, newdebgpt2) = rightSideHsApp e2 newcont1 newdebgpt1 func
				in (HsInfixApp newe1 op newe2, newcont2, newdebgpt2)
			HsDo lstmt 	     -> 
				let (newe1, newcont1, newdebgpt1) = exploreHsDo lstmt cont debgpt func
				in (HsDo newe1, newcont1, newdebgpt1) 
			HsIf       e1 e2 e3  -> 
				let (newe2, newcont1, newdebgpt1) = rightSideHsApp e2 cont debgpt func
				    (newe3, newcont2, newdebgpt2) = rightSideHsApp e3 newcont1 newdebgpt1 func
				in (HsIf e1 newe2 newe3, newcont2, newdebgpt2)
			HsParen e1           -> 
				let (newe1, newcont1, newdebgpt1) = rightSideHsApp e1 cont debgpt func
				in (HsParen newe1, newcont1, newdebgpt1)
			_		     -> (exp, cont, debgpt) 


functionNameHsApp hsapp = case hsapp of 
			HsApp e1 e2   -> functionNameHsApp e1
			HsVar (UnQual (HsIdent fxname)) -> fxname
			_		 -> "function name not found"	

newHsApp label dst func = 
  HsApp (HsApp (HsVar (UnQual (HsIdent func))) (HsLit (HsString label))) (HsParen ( dst ) )

posObserve = SrcLoc {srcFilename = "",  srcLine = 1, srcColumn = 0 }

addImportObserve limports lib = (HsImportDecl { importLoc = posObserve, importModule = Module lib, importQualified = False, importAs = Nothing, importSpecs = Nothing }) : limports 

{-
addImportObserve limports opts = (HsImportDecl { importLoc = posObserve, importModule = Module (if (snd opts) == [] then "Hugs.Observe" else (snd opts)), 
importQualified = False, importAs = Nothing, importSpecs = Nothing }) : limports 
-}

-- genFileDbgName :: (Show a) => [a] -> [a]
genFileDbgName fname = (reverse $ drop 3 (reverse fname)) ++ "_dbg.hs"

getExpr (a,_,_) = a 
getCont (_,b,_) = b
getDbgPnt (_,_,c) = c 



{-
--- Para gerar codigo anotado em mais de um nivel 
newMatch m@(HsMatch src nm pat (HsUnGuardedRhs exp) hsdcl) debugpts =  
	HsMatch src nm pat (HsUnGuardedRhs ( getExpr (rightSideHsApp exp 0 debugpts) ) ) hsdcl
-}











