module Main where

import DebugLocalizationCode
import DebugGenerationCode
import System.Posix.Process
import System.Environment

main = do 
       args <- getArgs
       if null args then 
	 doInteractive
        else 
	if (head args) == "-h" then 
	  doHelp
        else 
          doWithArgs args

doInteractive = do 
	    putStr "\n---------Haskell Debug vs 0.1------\n\n"
            putStr "Enter a file name and it path to debug: "
            usercode <- getLine
            showDebugPoints usercode
            putStr "\nChoose the points to put debug code - format x,y,z,...: "
            debgpnts <- getLine
            genDebugCode usercode (parseListInput debgpnts) ("","")
            putStr "\nCode ready to debuged!"
            putStr "\n\nStarting Debugg Session...\n"
            executeFile "/usr/bin/hugs" False [genFileDbgName usercode] Nothing
            return ()

doHelp = do 
            putStr "\nHaskell Debug vs 0.1\n"
            putStr "Usage: ControlDebug [-h] [path] [dbg_library] [debg_func]\n\n"
            putStr "   -h,          Display this help message\n"
            putStr "   path,        Where is the file to debug ? Default is interactive\n"
            putStr "   dbg_library, Debug library Default is Hugs.Observe\n"
            putStr "   debg_func,   Debug function Default is observe\n\n"
	    putStr "Obs: \n"
	    putStr "New file is generated in the same directory with name \"<file name>_dbg.hs\" : \n"
	    putStr "Please send bugs reports to dcc6rvc@joinville.udesc.br\n\n"		

doWithArgs args = do 	
            let (fpath,lparam) = (head args, tail args)
            let (lib,  lparam) = (if null lparam then "Hugs.Observe" else head lparam, tail lparam)
            let func = if null lparam then "observe" else head lparam
            showDebugPoints fpath
            putStr "\nChoose the points to put debug code - format x,y,z,...: "
            debgpnts <- getLine
	    genDebugCode fpath (parseListInput debgpnts) (lib, func)
            putStr "\nCode ready to debuged!"
            putStr "\n\nStarting Debugg Session...\n"
	    executeFile "/usr/bin/hugs" False [genFileDbgName fpath] Nothing
            return ()
         
parseListInput [] = []
parseListInput lst = let (l:ls) = lex lst 
		         cont = fst l 
		     in (if cont == "," then 
			  parseListInput (snd l)
			else
			  (read cont) : parseListInput (snd l) )
	
