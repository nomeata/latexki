module Latex ( procTex, texDeps ) where

import Maybe
import Monad
import System.Directory
import System.Process
import System.IO
import System
import Char
import List

import WikiData
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
findSimpleCommands ('\\':rest1) | null rest5					= [(command, "")]
				| n == '{'      				= (command,param) :findSimpleCommands rest3
				| n == '[' && (length rest3') > 2 && m == '{' 	= (command,param2):findSimpleCommands rest4
                                | otherwise          				= (command,""):findSimpleCommands rest5
--				| command == "printindex"			= ("printindex", ""):findSimpleCommands (n:rest2)
--                              | otherwise 					= findSimpleCommands (n:rest2)
	where (command,rest5)	= span isAlpha rest1
	      (n:rest2)		= rest5
	      (param,rest3)     = span (/='}') rest2
	      (_,rest3')        = span (/=']') rest2
	      (_:m:rest3'')     = rest3'
	      (param2,rest4)    = span (/='}') rest3''
findSimpleCommands (_:rest)                 									= findSimpleCommands rest	      


depCmds = [("input",[".tex",".part.tex"]),("include",[".tex",".part.tex"]),
		("usepackage",[".sty"]), ("includegraphics",["",".png"] ) ]
texDeps tex wi = liftM (map fileDep) $ texDeps' tex wi
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
	--copy f t =  putStrLn ("copying   " ++ f ++ " to " ++ t) >> copyFile f t
	copy f t = copyFile f t
	--strip f t = putStrLn ("stripping " ++ f ++ " to " ++ t) >> ((writeFileSafe t). strip' =<< readFile f)
	strip f t = ((writeFileSafe t). strip' =<< readFile f)
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
		ExitFailure _ -> putStr (show err ++ ", PDF deleted") >> safeRemoveFile pdffile
		ExitSuccess   -> putStr "ok"
	genHTML tex wi err
	return ()
  where realsource = backDir (pagename tex) ++ tex
  	realbasename = filename $ pagename tex
  	pdffile  = pagename tex ++ ".pdf"
  	runLatex = do
	prepareStripped realsource wi

	let env = [ ("TEXINPUTS",".:" ++ concatMap (++":") (dirTrail realsource)) ] -- colon to append, not override, default
	
	let runit c a = do
		let output = realbasename ++ ".output"
		appendFile output $ "\nRunning "++c++" "++(concat (intersperse " " a))++
				    " in an env with "++(show (length env))++" entries:\n"
  		readNull <- openFile "/dev/null" ReadMode
	  	writeLog <- openFile (realbasename  ++ ".output") AppendMode
		err <- runProcess c a Nothing (Just env) (Just readNull) (Just writeLog) (Just writeLog) >>=
			waitForProcess
		appendFile output $ "Result: "++(show err)++"\n"
		return err


	let clearOutput = do
		safeRemoveFile $ realbasename ++ ".output"
		return ExitSuccess

	usesPST' <- usesPST realsource wi
	let pstqueue = if usesPST'
			then [
				runit "/usr/bin/latex" [ realsource ],
				runit "/usr/bin/dvips" [ (realbasename ++ ".dvi") , "-o", (realbasename ++ "-pics.ps") ],
				runit "/usr/bin/ps2pdf" [ (realbasename ++ "-pics.ps") ]
	        	]
			else []
	
	usesIndex' <- usesIndex realsource wi
	let indexqueue = if usesIndex'
			then [
				runit "/usr/bin/makeindex" [ (realbasename ++ ".idx") ]
			]
			else []
	
	let latexrun = 		runit "/usr/bin/pdflatex" [realsource]

	let pngrun =		runit "/usr/bin/convert" [ "-verbose", realbasename++".pdf[0]", realbasename++".png" ] 
	
	return $[clearOutput] ++
		pstqueue ++
		[latexrun] ++
		indexqueue ++
		replicate 2 latexrun ++
		[pngrun]



genHTML tex wi err = do 
	index <- genIndex tex wi
	writeFileSafe target $ htmlPage wi tex title $ titleline ++ content ++ index ++ preview
 where  title = pagename tex
 	titleline = [Header 1 ("Latex File: "++ title)]
        content | err == ExitSuccess = [
			 Paragraph [Text "File successfully created:"],
			 ItemList [[LinkElem (PlainLink pdfFile "PDF-File")],
			           [LinkElem (PlainLink logFile "Latex-Logfile")],
			           [LinkElem (PlainLink outFile "Latex-Output")],
			           [LinkElem (PlainLink texFile "Latex-Source")]]
			]
                | otherwise          = [	
			 Paragraph [Text ("File not successfully created ("++(show err)++"):")],
			 ItemList [[Text "PDF-File (?)"],
			           [LinkElem (PlainLink logFile "Latex-Logfile")],
			           [LinkElem (PlainLink outFile "Latex-Output")],
			           [LinkElem (PlainLink texFile "Latex-Source")]]
			]
        preview | err == ExitSuccess = [
			 Header 2 "Preview",
			 Paragraph [Image pngFile "Preview"]
			]
                | otherwise          = [
			]
	pdfFile = (pagename tex) ++ ".pdf"				      
	logFile = (pagename tex) ++ ".log" 
	outFile = (pagename tex) ++ ".output" 
	pngFile = (pagename tex) ++ ".png"				      
	texFile = (backDir tex) ++ tex
	target  = (pagename tex) ++ ".html" 


findspans :: header -> (line -> Maybe header) -> [line] -> [((Int, Int), header)]
findspans _      _       []   = []
findspans first extract list = findspans' first list 1 0
  where	findspans' current []     a b = [ ((a,b),current) ]
        findspans' current (x:xs) a b = case extract x of
		Just new -> ((a,b), current) : findspans' new     xs (b+1) (b+1)
		Nothing  ->                    findspans' current xs  a    (b+1)

genIndex tex wi = return . format . extract . map uncomment . lines =<< readFile tex
  where	extract = findspans "Prelude" extract_chapter 
  	extract_chapter line = listToMaybe $ do (command,param) <- findSimpleCommands line
				                guard $ command =="chapter"
				                return param
	format = (Header 2 "Index-Preview":) . (:[]) . ItemList . map format'
	  where format' ((a,b),t) = [Text (t++" "),LinkElem (PlainLink (editLinkLines (pagename tex) a b) "(bearbeiten)")]

	
	

