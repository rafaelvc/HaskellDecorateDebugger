	
Cada HsDecl da lista contem o codigo de uma funcao;
O Tipo HsFunBind contem uma lista HsMatch
Cada HsMatch refere-se a um padrao para um determinada funcao.
O HsMatch é do tipo SrcLoc HsName [HsPat] HsRhs [HsDecl]	
HsName e o nome da funcao no padrao
HsPat lista de itens do padrao
HsRhs é o lado direito da funcao que pode ser do tipo  (HsUnGuardedRhs HsExp ou  HsGuardedRhss [HsGuardedRhs])
No momento estamos interessados no HsUnGuardedRhs HsExp, onde os tipos interessantes de HsExp
que representa a expressao sao HsApp (Aplicacao de funcao) e HsInfix do tipo (HsExp HsQOp HsExp), HsQOp representado operador qualificado

teste para verificar se a ateracao da estrutura srcloc inlfuencia no prettyPrint
Veriricar 
	

{-
			
(HsApp (HsApp (HsVar (UnQual (HsIdent "observe"))) (HsLit (HsString "fat"))) (HsParen ( dst ) )

HsApp (HsVar (UnQual (HsIdent "observe")))

HsApp (HsVar (UnQual (HsIdent "fat"))) (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "n"))) (HsQVarOp (UnQual (HsSymbol "-"))) (HsLit (HsInt 1))))))) 


(HsInfixApp (HsVar (UnQual (HsIdent "n"))) (HsQVarOp (UnQual (HsSymbol "-"))) (HsLit (HsInt 1)))

HsApp (HsVar (UnQual (HsIdent "observe")))

-} 

{- 

Lista de Padroes: 

[HsMatch (HsIdent "fat") [HsPLit (HsInt 0)] (HsUnGuardedRhs (HsLit (HsInt 1))) [],
HsMatch  (HsIdent "fat") [HsPVar (HsIdent "n")] (

HsInfixApp (operacao infixa)
HsApp (aplicacao de funcao)
HsParen (parenteses)

HsUnGuardedRhs (
	HsInfixApp 
	(HsVar (UnQual (HsIdent "n"))) 
	(HsQVarOp (UnQual (HsSymbol "*"))) 
	(HsApp (HsVar (UnQual (HsIdent "fat"))) 
		(HsParen (HsInfixApp (HsVar (UnQual (HsIdent "n"))) 
		(HsQVarOp (UnQual (HsSymbol "-"))) 
		(HsLit (HsInt 1))))))) []]



(HsApp (HsApp (HsApp (HsVar (UnQual (HsIdent "observe"))) (HsLit (HsString "fat"))) (HsVar (UnQual (HsIdent "fat")))) 

(HsParen )))
