module Latex ( procTex ) where

import Maybe
import Monad
import System.Directory
import System.Process
import System.IO
import System.FilePath
import System
import Char
import List
import qualified Data.ByteString.Lazy.Char8 as B

import WikiData
import Common
import HtmlStyle
import Dependencies
import PDF



whileOk []     = return ExitSuccess
whileOk (x:xs) = do
	res <- x
	case res of 
		ExitSuccess -> whileOk xs
		otherwise   -> return res

uncomment t | B.null t                          = B.empty
            | t == B.singleton '\\'             = t
	    | B.singleton '\\' `B.isPrefixOf` t = B.head (B.tail t) `B.cons` uncomment (B.tail (B.tail t))
	    | B.singleton '%'  `B.isPrefixOf` t = B.empty
	    | True                              = B.head t `B.cons` uncomment (B.tail t)
{-
uncomment ""            = ""
uncomment "\\"          = "\\"
uncomment ('\\':c:line) = '\\':c:uncomment line
uncomment ('%':_)       = ""
uncomment (c:line)      = c:uncomment line
-}

findSimpleCommands :: B.ByteString -> [ (B.ByteString, B.ByteString) ]
findSimpleCommands t | B.null t				= []
		     | B.singleton '\\' == t            = []
		     | B.pack "\\%" `B.isPrefixOf` t	= findSimpleCommands $ B.drop 2 t
		     | B.singleton '%' `B.isPrefixOf` t   = findSimpleCommands $ safeTail $ B.dropWhile (/= '\n') t
		     | B.singleton '\\' `B.isPrefixOf` t  = (command, param) : findSimpleCommands  rest
		         where	(command,optParamRest) = B.span isAlpha (B.tail t)
				paramRest | B.singleton '[' `B.isPrefixOf` optParamRest =
						safeTail $ B.dropWhile (/= ']') optParamRest
					  | otherwise  =
					  	optParamRest
				(param,rest) | B.singleton '{' `B.isPrefixOf` paramRest =
						let (param',rest') = B.span (/= '}') paramRest in (safeTail param', safeTail rest')
					     | otherwise =
					     	(B.empty,paramRest)
safeTail bs | B.null bs = bs
            | otherwise = B.tail bs
			        
{-
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
-}

depCmds = [("input",[".tex",".part.tex"]),("include",[".tex",".part.tex"]),
		("usepackage",[".sty"]), ("includegraphics",["",".png"] ) ]
{-
findDeps tex = do
	let file = B.unpack $ smContent tex
	let commands = findSimpleCommands file
	    candits = catMaybes $  map (\(c,f) -> case lookup c depCmds of 
	  			        		Just exts -> Just $ (f,exts)
							Nothing -> Nothing          ) commands
	files <- liftM (filter (/=tex).catMaybes) $ mapM find candits
	additional <- liftM (nub.sort.concat) $ mapM findDeps files
	return (tex:additional)
 where  find (candit,exts) = liftM listToMaybe $ filterM doesFileExist [ 
			dir </> candit <.> ext |
				dir <- dirTrail tex,
				ext <- exts
			]
-}	

{-
texInclCmds = ["input","include"]
prepareStripped :: PageInfo -> FileProducer ()
prepareStripped tex = do
	let file = B.unpack $ smContent tex
	let commands = findSimpleCommands file
	    candits = map snd $ filter (\(c,f) -> c `elem` texInclCmds) commands
	files <- liftIO $ liftM catMaybes $ mapM find candits
	sequence (map snd files)
	mapM_ (\(f,_) -> prepareStripped f) files
 where  find candit = liftM listToMaybe $ filterM (doesFileExist.fst) [ 
			(dir </> candit <.> suf, m (dir </> candit <.> suf) (candit <.> ".tex")) |
				dir <- dirTrail (fileRelative tex),
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
-}	


hasCommand command tex = command  `elem` findSimpleCommands (smContent tex)

usesPST = hasCommand (B.pack "usepackage", B.pack "pst-pdf")
usesIndex = hasCommand (B.pack "printindex", B.empty)


procTex :: FileProcessor
procTex tex = do
	let htmlFile = pageOutput tex "html"
	    pdfFile  = pageOutput tex "pdf"
	    pngFile  = pageOutput tex "png"
	--depRes <- needUpdates [htmlFile,pdfFile,pngFile] =<< findDeps tex
	--let up2date = isUpToDate depRes
	--liftIO $ showState (pagename tex) depRes
	return [ ( [htmlFile, pdfFile, pngFile], do
		
		ok <- genPDF tex 
		if ok then do
			pdfInfo <- liftIO $ getPDFInfo pdfFile 
			--splitPDF pdfFile pdfInfo >>= producedFiles
			genHTML tex ok (Just pdfInfo)
		 else 
			genHTML tex ok Nothing
		) ]


genPDF :: PageInfo -> FileProducer (Bool)
genPDF tex =  do 
	--prepareStripped tex
	err <- liftIO $ whileOk =<< runLatex
	case err of
		ExitFailure _ -> liftIO $ putStrLn (pagename tex ++ ": LaTeX failed ("++show err ++")")
		ExitSuccess   -> return ()
	return $ err == ExitSuccess
  where realsource = fileRelative tex
  	--realbasename = dropExtensions realsource
	output   = pageOutput tex "output"
	outDir   = takeDirectory (pagename tex)
  	pdffile  = pageOutput tex "pdf"
  	runLatex = do

	let env = [ ("TEXINPUTS",".:" ++ concatMap (++":") (dirTrail realsource)) ] -- colon to append, not override, default
	
	let runit dir c a = do
		--let output = realbasename ++ ".output"
		appendFile output $ "\nRunning "++c++" "++(concat (intersperse " " a))++
				    " in an env with "++(show (length env))++" entries:\n"
  		readNull <- openFile "/dev/null" ReadMode
	  	writeLog <- openFile output AppendMode
		err <- inDir dir $
			runProcess c a Nothing (Just env) (Just readNull) (Just writeLog) (Just writeLog) >>=
			waitForProcess
		appendFile output $ "Result: "++(show err)++"\n"
		return err


	let clearOutput = do
		safeRemoveFile $ output
		return ExitSuccess

	let pstqueue = if usesPST tex
			then [
				runit outDir "/usr/bin/latex" [ realsource ],
				runit ""     "/usr/bin/dvips" [ pageOutput tex "dvi", "-o", pageOutput tex "pics.ps" ],
				runit ""     "/usr/bin/ps2pdf" [ pageOutput tex "pics.ps" ]
	        	]
			else []
	
	let indexqueue = if usesIndex tex
			then [
				runit ""     "/usr/bin/makeindex" [ pageOutput tex "idx" ]
			]
			else []
	
	let latexrun = 		runit outDir "/usr/bin/pdflatex" [realsource]

	let pngrun =		runit ""     "/usr/bin/convert" [ "-verbose", pdffile ++ "[0]", pageOutput tex "png" ] 
	
	return $ [clearOutput] ++
		 pstqueue ++
		 [latexrun] ++
		 indexqueue ++
		 replicate 2 latexrun ++
		 [pngrun]

genHTML tex ok pdfInfo = do 
	let source = smContent tex
	let index = getIndex tex source
	    title = getTitle tex source
	    titleline = [Header 1 (B.pack "Latex File: " `B.append` title)]
	writeHtmlPage target tex title $ titleline ++ content ++ index ++ pdfIndex ++ preview
  where pdfIndex = case pdfInfo of
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
	pdfFile = pageOutput tex "pdf"
	logFile = pageOutput tex "log"
	outFile = pageOutput tex "output"
	pngFile = pageOutput tex "png"
	target  = pageOutput tex "html"
	texFile = fileRelative tex


findspans :: header -> (line -> Maybe header) -> [line] -> [((Int, Int), header)]
findspans _      _       []   = []
findspans first extract list = findspans' first (map extract list) 1 0
  where	findspans' current []            a b = [((a,b), current)]
        findspans' current (Just new:xs) a b =  ((a,b), current) : findspans' new     xs (b+1) (b+1)
        findspans' current (Nothing :xs) a b =                     findspans' current xs  a    (b+1)

getIndex tex = format . extract . map uncomment . B.lines 
  where	extract = findspans (B.pack "Preamble") extract_chapter 
  	extract_chapter line = listToMaybe $ do (command,param) <- findSimpleCommands line
				                guard $ command == B.pack "chapter"
				                return param
	format = (Header 2 "Index-Preview":) . (:[]) . ItemList . map format'
	  where format' ((a,b),t) = [
	  				Text (t++" "),
					LinkElem (PlainLink (editLinkLines tex a b) "(bearbeiten)"
					)]

getTitle tex = fromMaybe (B.pack (pagename tex)) . lookup (B.pack "title") . findSimpleCommands  

