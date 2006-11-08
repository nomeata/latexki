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
	
findSimpleCommands ""                       					= []
findSimpleCommands ('\\':rest1) | n == '{'      				= (command,param) :findSimpleCommands rest3
				| n == '[' && (length rest3') > 2 && m == '{' 	= (command,param2):findSimpleCommands rest4
                                | otherwise          				= (command,""):findSimpleCommands (n:rest2)
--				| command == "printindex"			= ("printindex", ""):findSimpleCommands (n:rest2)
--                              | otherwise 					= findSimpleCommands (n:rest2)
	where (command,n:rest2) = span isAlpha rest1
	      (param,rest3)     = span (/='}') rest2
	      (_,rest3')        = span (/=']') rest2
	      (_:m:rest3'')     = rest3'
	      (param2,rest4)    = span (/='}') rest3'
findSimpleCommands (_:rest)                 									= findSimpleCommands rest	      


depCmds = [("input",[".tex",".part.tex"]),("include",[".tex",".part.tex"]),
		("usepackage",[".sty"]), ("includegraphics",["",".png"] ) ]
texDeps tex wi = liftM (map FileDep) $ texDeps' tex wi
texDeps' tex wi = do
	file' <- readFile tex
	let file = (unlines.(map uncomment).lines) file'
	    commands = findSimpleCommands file
	    candits = catMaybes $  map (\(c,f) -> case lookup c depCmds of 
	  			        		Just exts -> Just $ (f,exts)
							Nothing -> Nothing          ) commands
	files <- liftM (filter (/=tex).catMaybes) $ mapM find candits
	additional <- liftM (nub.sort.concat) $ mapM (\d -> texDeps' d wi) files
	return (tex:additional)
 where  addpath = map ((dirname tex++"/") ++ )
	find (candit,exts) = liftM listToMaybe $ filterM (doesFileExist) [ 
			dir ++ "/" ++ candit ++ ext |
				dir <- dirTrail tex,
				ext <- exts
			]

texInclCmds = ["input","include"]
prepareStripped tex wi = do
	file' <- readFile tex
	let file = (unlines.(map uncomment).lines) file'
	    commands = findSimpleCommands file
	    candits = map snd $ filter (\(c,f) -> c `elem` texInclCmds) commands
	files <- liftM catMaybes $ mapM find candits
	sequence (map snd files)
	mapM (\(f,_) -> prepareStripped f wi) files
	return ()
 where  addpath = map (datadir++)
	find candit = liftM listToMaybe $ filterM (doesFileExist.fst) [ 
			(dir ++ "/" ++ candit ++ suf, m (dir ++ "/" ++ candit ++ suf) (candit ++ ".tex")) |
				dir <- dirTrail tex,
				(suf,m) <- methods
			]
	methods = [ (".part.tex", copy), (".tex",strip) ]
	copy f t =  putStrLn ("copying   " ++ f ++ " to " ++ t) >> copyFile f t
	strip f t = putStrLn ("stripping " ++ f ++ " to " ++ t) >> ((writeFileSafe t). strip' =<< readFile f)
	 where 
	 	strip' file = chaptertitle $ mainPart file 
		 where	title = fromMaybe "No Title" $ lookup "title" $ findSimpleCommands file
		 	chaptertitle = replace "\\maketitle" ("\\chapter{"++title++"}")
		mainPart = unlines .  stail . takeWhile (not. subListOf "\\end{document}") . stail . dropWhile (not. subListOf "\\begin{document}") . lines
	        stail [] = []
	        stail l  = tail l
	

usesPST tex wi = do
	file' <- readFile tex
	let file = (unlines.(map uncomment).lines) file'
	return $ ("usepackage", "pst-pdf")  `elem` (findSimpleCommands file)
	
usesIndex tex wi = do
	file' <- readFile tex
	let file = (unlines.(map uncomment).lines) file'
	return $ ("printindex", "")  `elem` (findSimpleCommands file)

procTex tex wi = do
	cwd <- getCurrentDirectory
	safeChdir (dirname $ pagename tex)
	err <- whileOk =<< runLatex
	setCurrentDirectory cwd
	case err of
		ExitFailure _ -> putStr (show err ++ ", PDF deleted") >> removeFileIfExists pdffile
		ExitSuccess   -> putStr "ok"
	genHTML tex wi err
	return ()
  where realsource = backDir (pagename tex) ++ tex
  	realbasename = filename $ pagename tex
  	pdffile  = pagename tex ++ ".pdf"
  	removeFileIfExists file = do exists <- doesFileExist file ; if exists then removeFile file else return ()
  	runLatex = do
	prepareStripped realsource wi

	let env = [ ("TEXINPUTS",".:" ++ concatMap (++":") (dirTrail realsource)) ] -- colon to append, not override, default
	
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
	
	usesIndex' <- usesIndex realsource wi
	let indexqueue = if usesIndex'
			then [
				runit "makeindex" [ (realbasename ++ ".idx") ]
			]
			else []
	
	return $ pstqueue ++ [(runit "/usr/bin/pdflatex" [realsource])] ++ indexqueue ++ replicate 2 (runit "/usr/bin/pdflatex" [realsource])



genHTML tex wi err = do 
	writeFileSafe target $ htmlPage wi tex (pagename tex) $ title ++ content
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


