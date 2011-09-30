import Observe 

main = runO example 


example :: IO () 
example = print
         ((observe "length" :: Observing ([Int] -> Int))
        	length [1..3]
    	 )


{- 
import Hugs.Observe

example :: IO () 
example = print ( observe "length" length [1..3] )
-} 

{- 


sModule SrcLoc {srcFilename = "<unknown>", srcLine = 1, srcColumn = 1} (Module "Main") (Just [HsEVar (UnQual (HsIdent "main"))]) [HsImportDecl {importLoc = SrcLoc {srcFilename = "<unknown>", srcLine = 1, srcColumn = 1}, importModule = Module "Observe", importQualified = False, importAs = Nothing, importSpecs = Nothing}] 

[HsTypeSig SrcLoc [HsIdent "example"] (HsQualType [] (HsTyApp (HsTyCon (UnQual (HsIdent "IO"))) (HsTyCon (Special HsUnitCon)))),HsPatBind SrcLoc  (HsPVar (HsIdent "example")) (HsUnGuardedRhs 
(HsApp (HsVar (UnQual (HsIdent "print"))) 
	(HsParen
(HsApp 
	 (HsApp 
		( HsParen ( HsExpTypeSig SrcLoc (HsApp (HsVar (UnQual (HsIdent "observe"))) (HsLit (HsString "length")) ) 

(HsQualType [] (HsTyApp (HsTyCon (UnQual (HsIdent "Observing"))) 

(HsTyFun (HsTyApp (HsTyCon (Special HsListCon)) (HsTyCon (UnQual (HsIdent "Int")))) (HsTyCon (UnQual (HsIdent "Int")))))))) (HsVar (UnQual (HsIdent "length")))) (HsEnumFromTo (HsLit (HsInt 1)) (HsLit (HsInt 3))))))) []]


HsTyFun = HsType HsType

HsTyApp = HsType HsType


(HsQualType [] (HsTyApp (HsTyCon (UnQual (HsIdent "Observing"))) 

(HsTyFun (HsTyApp (HsTyCon (Special HsListCon)) (HsTyCon (UnQual (HsIdent "Int")))) (HsTyCon (UnQual (HsIdent "Int"))))))))



HsExp = HsExpTypeSign SrcLoc HsExp HsQualType 



-} 
