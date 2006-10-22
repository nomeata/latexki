module Latex ( procTex, texDeps ) where

import Maybe
import Monad
import System.Directory
import System.Process
import System.IO
import System
import Char
import List
import Common
import HtmlStyle

replicateCmd 0 cmd = return ExitSuccess
replicateCmd n cmd = do
	res <- cmd
	case res of
		ExitSuccess  -> replicateCmd (n-1) cmd
		otherwise    -> return res

whileOk []     = return ExitSuccess
whileOk (x:xs) = do
	res <- x
	case res of 
		ExitSuccess -> whileOk xs
		otherwise   -> return res
		

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
texDeps tex wi = liftM (map FileDep) $ texDeps' tex wi
texDeps' tex wi = do
	file' <- readFile tex
	let file = (unlines.(map uncomment).lines) file'
	    commands = findSimpleCommands file
	    candits = catMaybes $  map (\(c,f) -> case lookup c depCmds of 
	  			        		Just ext -> Just $ f++ext
							Nothing -> Nothing          ) commands
	    files = addpath candits
	existing <- exist files
	additional <- liftM (nub.sort) $ exist.concat =<< mapM (\d -> texDeps' d wi) existing
	return (tex:additional)
 where  addpath = map (datadir++)
        exist   = filterM doesFileExist . filter (/= tex)

-- Needs directory support!
texInclCmds = ["input","include"]
prepareStripped tex wi = do
	file' <- readFile tex
	let file = (unlines.(map uncomment).lines) file'
	    commands = findSimpleCommands file
	    candits = map snd $ filter (\(c,f) -> c `elem` texInclCmds) commands
	file <- exist candits
	mapM handle candits 
	mapM (\t -> prepareStripped t wi) =<< exist (addpath candits)
	return ()
 where  addpath = map (datadir++)
        exist   = filterM doesFileExist . filter (/= tex)
	methods = [ (".part.tex", copy), (".tex",strip) ]
	handle base = do methods' <- filterM (\(s,_) -> doesFileExist (datadir ++ base ++ s)) methods
			 case methods' of
		      	  []      -> return ()
			  [(s,m)] -> m (datadir ++ base ++ s) (base ++ ".tex")
	copy f t = copyFile f t
	strip f t = (writeFile t). strip' =<< readFile f
	 where 
	 	strip' file = chaptertitle $ mainPart file 
		 where	title = fromMaybe "No Title" $ lookup "title" $ findSimpleCommands file
		 	chaptertitle = replace "\\maketitle" ("\\chapter{"++title++"}")
		mainPart = unlines .  stail . takeWhile (not. subListOf "\\end{document}") . stail . dropWhile (not. subListOf "\\begin{document}") . lines
	        stail [] = []
	        stail l  = tail l
	

usesPST tex wi = do
	file <- readFile tex
	return $ ("usepackage", "pst-pdf")  `elem` (findSimpleCommands file)
	

procTex tex wi = do
	cwd <- getCurrentDirectory
	safeChdir (dirname $ pagename tex)
	putStrLn . ( "In dir "++) =<< getCurrentDirectory
	err <- whileOk =<< runLatex
	case err of
		ExitFailure _ -> debug wi (show err ++ ", PDF deleted") >> removeFileIfExists pdffile
		ExitSuccess   -> debug wi "ok"
	setCurrentDirectory cwd
	putStrLn . ( "In dir "++) =<< getCurrentDirectory
	genHTML tex wi err
	return ()
  where realsource = concat (replicate (length $ filter (=='/') $ pagename tex) "../") ++ tex
  	realbasename = filename $ pagename tex
  	pdffile  = pagename tex ++ ".pdf"
  	removeFileIfExists file = do exists <- doesFileExist file ; if exists then removeFile file else return ()
  	runLatex = do
	prepareStripped realsource wi

	let env = [ ("TEXINPUTS",".:" ++ concatMap (++":") (dirTrail tex)) ] -- colon to append, not override, default
	let runit c a = do
  		readNull <- return.Just =<< openFile "/dev/null" ReadMode
	  	writeLog <- return.Just =<< openFile (realbasename  ++ ".output") WriteMode
		runProcess c a Nothing (Just env) readNull writeLog writeLog >>= waitForProcess
	
	usesPST' <- usesPST realsource wi
	let pstqueue = if usesPST'
			then [
				runit "latex" [ realsource ],
				runit "dvips" [ (realbasename ++ ".dvi") , "-o", (realbasename ++ "-pics.ps") ],
				runit "ps2pdf" [ (realbasename ++ "-pics.ps") ]
	        	]
			else []
	
	return $ pstqueue ++ replicate 3 (runit "/usr/bin/pdflatex" [realsource])



genHTML tex wi err = do 
	writeFile target $ htmlPage wi tex (pagename tex) $ title ++ content
 where  title = tag "h1" ("Latex File: "++tex)
        content | err == ExitSuccess = 	"File successfully created:" ++
	           (tag "ul" (concat [(tag "li" (aHref pdfFile "PDF-File")),
	                              (tag "li" (aHref logFile "Latex-Logfile")),
	                              (tag "li" (aHref (datadir++tex) "Latex-Source"))]))
                | otherwise          = 	"File not successfully created ("++(show err)++":"++
	           (tag "ul" (concat [(tag "li" (aHref pdfFile "PDF-File?")),
	                              (tag "li" (aHref logFile "Latex-Logfile")),
	                              (tag "li" (aHref tex     "Latex-Source"))]))
	pdfFile = (pagename tex) ++ ".pdf"				      
	logFile = (pagename tex) ++ ".log" 
	target  = (pagename tex) ++ ".html" 


