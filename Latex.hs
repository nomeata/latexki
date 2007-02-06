module Latex ( procTex ) where

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
import Dependencies
import PDF

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
findSimpleCommands ('\\':'%':rest) 						= findSimpleCommands rest
findSimpleCommands ('%':rest) 							= findSimpleCommands rest'
	where (_,rest')		= span (/='\n') rest
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
findSimpleCommands (_:rest)                       				= findSimpleCommands rest	      


depCmds = [("input",[".tex",".part.tex"]),("include",[".tex",".part.tex"]),
		("usepackage",[".sty"]), ("includegraphics",["",".png"] ) ]
findDeps tex = do
	file <- liftIO $ readFile tex
	let commands = findSimpleCommands file
	    candits = catMaybes $  map (\(c,f) -> case lookup c depCmds of 
	  			        		Just exts -> Just $ (f,exts)
							Nothing -> Nothing          ) commands
	files <- liftM (filter (/=tex).catMaybes) $ mapM find candits
	additional <- liftM (nub.sort.concat) $ mapM findDeps files
	return (tex:additional)
 where  addpath = map ((dirname tex++"/") ++ )
	find (candit,exts) = liftM listToMaybe $ filterM doesExist [ 
			dir ++ "/" ++ candit ++ ext |
				dir <- dirTrail tex,
				ext <- exts
			]

texInclCmds = ["input","include"]
prepareStripped tex = do
	file <- liftIO $ readFile tex
	let commands = findSimpleCommands file
	    candits = map snd $ filter (\(c,f) -> c `elem` texInclCmds) commands
	files <- liftM catMaybes $ mapM find candits
	sequence (map snd files)
	mapM (\(f,_) -> prepareStripped f) files
	return ()
 where  addpath = map (datadir++)
	find candit = liftM listToMaybe $ filterM (doesExist.fst) [ 
			(dir ++ "/" ++ candit ++ suf, m (dir ++ "/" ++ candit ++ suf) (candit ++ ".tex")) |
				dir <- dirTrail tex,
				(suf,m) <- methods
			]
	methods = [ (".part.tex", copy), (".tex",strip) ]
	--copy f t =  putStrLn ("copying   " ++ f ++ " to " ++ t) >> copyFile f t
	copy f t = liftIO $ copyFile f t
	--strip f t = putStrLn ("stripping " ++ f ++ " to " ++ t) >> ((writeFileSafe t). strip' =<< readFile f)
	strip f t = liftIO $ writeFileSafe t . strip' =<< readFile f
	 where 
	 	strip' file = chaptertitle $ mainPart file 
		 where	title = fromMaybe "No Title" $ lookup "title" $ findSimpleCommands file
		 	chaptertitle = replace "\\maketitle" ("\\chapter{"++title++"}")
		mainPart = unlines . takeWhile (not. subListOf "\\end{document}") . stail . dropWhile (not. subListOf "\\begin{document}") . lines
	        stail [] = []
	        stail l  = tail l
	


hasCommand command tex = liftIO $ do
	file <- readFile tex
	return $ command  `elem` (findSimpleCommands file)

usesPST = hasCommand ("usepackage", "pst-pdf")
usesIndex = hasCommand ("printindex", "")


procTex :: FileProcessor
procTex tex = do
	let htmlFile = pagename tex ++ ".html"
	    pdfFile  = pagename tex ++ ".pdf"
	    pngFile  = pagename tex ++ ".png"
	depRes <- needUpdates [htmlFile,pdfFile,pngFile] =<< findDeps tex
	let up2date = isUpToDate depRes
	liftIO $ showState (pagename tex) depRes
	if not up2date then do
		ok <- genPDF tex 
		when ok $ producedFile pdfFile
		when ok $ producedFile pngFile
		if ok then do
			pdfInfo <- liftIO $ getPDFInfo pdfFile 
			splitPDF pdfFile pdfInfo >>= producedFiles
			genHTML tex ok (Just pdfInfo)
		 else 
			genHTML tex ok Nothing
		producedFile htmlFile
	   else do
		producedFile pdfFile
		producedFile pngFile
		producedFile htmlFile
		guessSplitPDF pdfFile >>= producedFiles
	return ()


genPDF tex =  do 
	cwd <- liftIO $ getCurrentDirectory
	liftIO $ safeChdir (dirname $ pagename tex)
	prepareStripped realsource
	err <- liftIO $ whileOk =<< runLatex
	liftIO $ setCurrentDirectory cwd
	case err of
		ExitFailure _ -> liftIO $ putStrLn (pagename tex ++ ": LaTeX failed ("++show err ++")")
		ExitSuccess   -> return ()
	return $ err == ExitSuccess
  where realsource = backDir (pagename tex) ++ tex
  	realbasename = filename $ pagename tex
  	pdffile  = pagename tex ++ ".pdf"
  	runLatex = do

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

	usesPST' <- usesPST realsource
	let pstqueue = if usesPST'
			then [
				runit "/usr/bin/latex" [ realsource ],
				runit "/usr/bin/dvips" [ (realbasename ++ ".dvi") , "-o", (realbasename ++ "-pics.ps") ],
				runit "/usr/bin/ps2pdf" [ (realbasename ++ "-pics.ps") ]
	        	]
			else []
	
	usesIndex' <- usesIndex realsource 
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



genHTML tex ok pdfInfo = do 
	index <- liftIO $ genIndex tex 
	writeHtmlPage target tex title $ titleline ++ content ++ index ++ pdfIndex ++ preview
 where  title = pagename tex
 	titleline = [Header 1 ("Latex File: "++ title)]
	pdfIndex = case pdfInfo of
		Nothing -> []
		Just info -> formatPDFInfo pdfFile info
        content | ok = [
			 Paragraph [Text "File successfully created:"],
			 ItemList [[LinkElem (PlainLink pdfFile "PDF-File")],
			           [LinkElem (PlainLink logFile "Latex-Logfile")],
			           [LinkElem (PlainLink outFile "Latex-Output")],
			           [LinkElem (PlainLink texFile "Latex-Source")]]
			]
                | otherwise          = [	
			 Paragraph [Text ("File not successfully created:")],
			 ItemList [[Text "PDF-File (?)"],
			           [LinkElem (PlainLink logFile "Latex-Logfile")],
			           [LinkElem (PlainLink outFile "Latex-Output")],
			           [LinkElem (PlainLink texFile "Latex-Source")]]
			]
        preview | ok = [
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
findspans first extract list = findspans' first (map extract list) 1 0
  where	findspans' current []            a b = [((a,b), current)]
        findspans' current (Just new:xs) a b =  ((a,b), current) : findspans' new     xs (b+1) (b+1)
        findspans' current (Nothing :xs) a b =                     findspans' current xs  a    (b+1)

genIndex tex = return . format . extract . map uncomment . lines =<< readFile tex
  where	extract = findspans "Preamble" extract_chapter 
  	extract_chapter line = listToMaybe $ do (command,param) <- findSimpleCommands line
				                guard $ command =="chapter"
				                return param
	format = (Header 2 "Index-Preview":) . (:[]) . ItemList . map format'
	  where format' ((a,b),t) = [Text (t++" "),LinkElem (PlainLink (editLinkLines (pagename tex) a b) "(bearbeiten)")]

	
	

