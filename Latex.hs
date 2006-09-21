module Latex ( tex2pdf ) where

import Maybe
import System.Process
import System.IO
import System

replicateCmd 0 cmd = return ExitSuccess
replicateCmd n cmd = do
	res <- cmd
	case res of
		ExitSuccess  -> replicateCmd (n-1) cmd
		otherwise    -> return res

tex2pdf wi tex pdf = do
	err <- replicateCmd 3 runLatex
	putStrLn $ "Result: "++(show err)
	return ()
  where runLatex = do
  	readNull <- return.Just =<< openFile "/dev/null" ReadMode
  	writeNull <- return.Just =<< openFile "/dev/null" WriteMode
  	runProcess "pdflatex" [tex] Nothing Nothing readNull writeNull writeNull >>=
		waitForProcess
