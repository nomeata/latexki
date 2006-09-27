module Latex ( procTex, texDeps ) where

import Maybe
import Monad
import Directory
import System.Process
import System.IO
import System
import Char
import Common

replicateCmd 0 cmd = return ExitSuccess
replicateCmd n cmd = do
	res <- cmd
	case res of
		ExitSuccess  -> replicateCmd (n-1) cmd
		otherwise    -> return res

uncomment ""            = ""
uncomment "\\"          = "\\"
uncomment ('\\':c:line) = '\\':c:uncomment line
uncomment ('%':_)       = ""
uncomment (c:line)      = c:uncomment line
	
findSimpleCommands ""                       = []
findSimpleCommands ('\\':rest1) | n == '{'  = (command,param):findSimpleCommands rest3
                                | otherwise =                 findSimpleCommands (n:rest2)
	where (command,n:rest2) = span (isAlpha) rest1
	      (param,rest3)     = span (/='}')   rest2
findSimpleCommands (_:rest)                 =                 findSimpleCommands rest	      

depCmds = [("input",".tex"),("include",".tex"),("usepackage",".sty")]
texDeps tex wi = do
	file' <- readFile tex
	let file = (unlines.(map uncomment).lines) file'
	    commands = findSimpleCommands file
	    candits = catMaybes $  map (\(c,f) -> case lookup c depCmds of 
	  			        		Just ext -> Just $ f++ext
							Nothing -> Nothing          ) commands
	    files = map ((datadir++)) candits
	existing <- filterM (doesFileExist) files
	return existing

procTex tex wi = do
	err <- replicateCmd 3 runLatex
	putStrLn $ "Result: "++(show err)
	return ()
  where runLatex = do
  	readNull <- return.Just =<< openFile "/dev/null" ReadMode
  	writeNull <- return.Just =<< openFile "/dev/null" WriteMode
  	runProcess "pdflatex" [tex] Nothing Nothing readNull writeNull writeNull >>=
		waitForProcess


