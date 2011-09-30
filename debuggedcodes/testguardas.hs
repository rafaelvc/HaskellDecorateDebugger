module Tguardas where 

teste x | (x == 1) = 1
	| (x == 2) = 2
	| (x == 3) = 3
	| otherwise = teste2 (teste 2)

teste2 x = gx $ gy $ gy x

gx x = 1 

gy y = 1


zx y = do {
		putStr ("aa" ++ y);
	}
	

{-
HsModule SrcLoc {srcFilename = "<unknown>", srcLine = 5, srcColumn = 1} (Module "Main") (Just [HsEVar (UnQual (HsIdent "main"))]) [] [HsFunBind [HsMatch SrcLoc {srcFilename = "<unknown>", srcLine = 5, srcColumn = 1} (HsIdent "teste") [HsPVar (HsIdent "x")] (HsGuardedRhss [HsGuardedRhs SrcLoc {srcFilename = "<unknown>", srcLine = 5, srcColumn = 9} (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "x"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 1)))) (HsLit (HsInt 1)),HsGuardedRhs SrcLoc {srcFilename = "<unknown>", srcLine = 6, srcColumn = 9} (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "x"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 2)))) (HsLit (HsInt 2)),HsGuardedRhs SrcLoc {srcFilename = "<unknown>", srcLine = 7, srcColumn = 9} (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "x"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 3)))) (HsLit (HsInt 3))]) []]]
-}


{-
HsModule SrcLoc {srcFilename = "<unknown>", srcLine = 5, srcColumn = 1} (Module "Main") (Just [HsEVar (UnQual (HsIdent "main"))]) [] [HsFunBind [HsMatch SrcLoc (HsIdent "teste") [HsPVar (HsIdent "x")] (HsGuardedRhss [HsGuardedRhs SrcLoc (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "x"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 1)))) (HsLit (HsInt 1)), HsGuardedRhs SrcLoc (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "x"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 2)))) (HsLit (HsInt 2)),HsGuardedRhs SrcLoc (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "x"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 3)))) (HsLit (HsInt 3))]) []],HsFunBind [HsMatch SrcLoc (HsIdent "teste2") [HsPVar (HsIdent "x")] (HsUnGuardedRhs (HsLit (HsInt 2))) []]]



HsModule SrcLoc {srcFilename = "<unknown>", srcLine = 1, srcColumn = 1} (Module "Tguardas") Nothing [] [HsFunBind [HsMatch SrcLoc {srcFilename = "<unknown>", srcLine = 3, srcColumn = 1} (HsIdent "teste") [HsPVar (HsIdent "x")] (HsGuardedRhss [HsGuardedRhs SrcLoc {srcFilename = "<unknown>", srcLine = 3, srcColumn = 9} (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "x"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 1)))) (HsLit (HsInt 1)),HsGuardedRhs SrcLoc {srcFilename = "<unknown>", srcLine = 4, srcColumn = 9} (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "x"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 2)))) (HsLit (HsInt 2)),HsGuardedRhs SrcLoc {srcFilename = "<unknown>", srcLine = 5, srcColumn = 9} (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "x"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 3)))) (HsLit (HsInt 3)),HsGuardedRhs SrcLoc {srcFilename = "<unknown>", srcLine = 6, srcColumn = 9} (HsVar (UnQual (HsIdent "otherwise"))) (HsApp (HsVar (UnQual (HsIdent "teste2"))) (HsParen (HsApp (HsVar (UnQual (HsIdent "teste"))) (HsLit (HsInt 2)))))]) []],HsFunBind [HsMatch SrcLoc {srcFilename = "<unknown>", srcLine = 8, srcColumn = 1} (HsIdent "teste2") [HsPVar (HsIdent "x")] (HsUnGuardedRhs (HsInfixApp (HsVar (UnQual (HsIdent "gx"))) (HsQVarOp (UnQual (HsSymbol "$"))) (HsApp (HsVar (UnQual (HsIdent "gy"))) (HsVar (UnQual (HsIdent "x")))))) []],HsFunBind [HsMatch SrcLoc {srcFilename = "<unknown>", srcLine = 10, srcColumn = 1} (HsIdent "gx") [HsPVar (HsIdent "x")] (HsUnGuardedRhs (HsLit (HsInt 1))) []],HsFunBind [HsMatch SrcLoc {srcFilename = "<unknown>", srcLine = 12, srcColumn = 1} (HsIdent "gy") [HsPVar (HsIdent "y")] (HsUnGuardedRhs (HsLit (HsInt 1))) []]]





HsModule SrcLoc {srcFilename = "<unknown>", srcLine = 1, srcColumn = 1} (Module "Tguardas") Nothing [] [HsFunBind [HsMatch SrcLoc {srcFilename = "<unknown>", srcLine = 3, srcColumn = 1} (HsIdent "teste") [HsPVar (HsIdent "x")] (HsGuardedRhss [HsGuardedRhs SrcLoc {srcFilename = "<unknown>", srcLine = 3, srcColumn = 9} (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "x"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 1)))) (HsLit (HsInt 1)),HsGuardedRhs SrcLoc {srcFilename = "<unknown>", srcLine = 4, srcColumn = 9} (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "x"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 2)))) (HsLit (HsInt 2)),HsGuardedRhs SrcLoc {srcFilename = "<unknown>", srcLine = 5, srcColumn = 9} (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "x"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 3)))) (HsLit (HsInt 3)),HsGuardedRhs SrcLoc {srcFilename = "<unknown>", srcLine = 6, srcColumn = 9} (HsVar (UnQual (HsIdent "otherwise"))) (HsApp (HsVar (UnQual (HsIdent "teste2"))) (HsParen (HsApp (HsVar (UnQual (HsIdent "teste"))) (HsLit (HsInt 2)))))]) []],HsFunBind [HsMatch SrcLoc {srcFilename = "<unknown>", srcLine = 8, srcColumn = 1} (HsIdent "teste2") [HsPVar (HsIdent "x")] (HsUnGuardedRhs (HsInfixApp (HsInfixApp (HsVar (UnQual (HsIdent "gx"))) (HsQVarOp (UnQual (HsSymbol "$"))) (HsVar (UnQual (HsIdent "gy")))) (HsQVarOp (UnQual (HsSymbol "$"))) (HsApp (HsVar (UnQual (HsIdent "gy"))) (HsVar (UnQual (HsIdent "x")))))) []],HsFunBind [HsMatch SrcLoc {srcFilename = "<unknown>", srcLine = 10, srcColumn = 1} (HsIdent "gx") [HsPVar (HsIdent "x")] (HsUnGuardedRhs (HsLit (HsInt 1))) []],HsFunBind [HsMatch SrcLoc {srcFilename = "<unknown>", srcLine = 12, srcColumn = 1} (HsIdent "gy") [HsPVar (HsIdent "y")] (HsUnGuardedRhs (HsLit (HsInt 1))) []],HsFunBind [HsMatch SrcLoc {srcFilename = "<unknown>", srcLine = 15, srcColumn = 1} (HsIdent "zx") [HsPVar (HsIdent "y")] (HsUnGuardedRhs (HsDo [HsQualifier (HsApp (HsVar (UnQual (HsIdent "putStr"))) (HsParen (HsInfixApp (HsLit (HsString "aa")) (HsQVarOp (UnQual (HsSymbol "++"))) (HsVar (UnQual (HsIdent "y"))))))])) []]]



-}
