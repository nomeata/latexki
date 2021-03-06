{-# LANGUAGE RecordWildCards #-}
module Latex ( procTex, depsTex ) where

import Data.Maybe
import System.Directory
import System.Process
import System.IO
import System.FilePath
import System.Exit
import System.Environment
import Control.Monad
import Data.Char
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B

import WikiData
import Common
import HtmlStyle
import PDF
import GIT

whileOk []     = return ExitSuccess
whileOk (x:xs) = do
        res <- x
        case res of 
                ExitSuccess -> whileOk xs
                otherwise   -> return res

uncomment t = case filter (\n -> n==0 || B.index t (n-1) /= '\\') (B.elemIndices '%' t) of
                []  -> t
                n:_ -> B.take n t

{-
uncomment t | B.null t                          = B.empty
            | t == B.singleton '\\'             = t
            | B.singleton '\\' `B.isPrefixOf` t = B.cons (B.head t) $ B.cons (B.head (B.tail t)) $ uncomment (B.tail (B.tail t))
            | B.singleton '%'  `B.isPrefixOf` t = B.empty
            | otherwise                         = B.head t `B.cons` uncomment (B.tail t)
-}

{-
uncomment ""            = ""
uncomment "\\"          = "\\"
uncomment ('\\':c:line) = '\\':c:uncomment line
uncomment ('%':_)       = ""
uncomment (c:line)      = c:uncomment line
-}

findSimpleCommands :: B.ByteString -> [ (B.ByteString, B.ByteString) ]
findSimpleCommands t | B.null t                          = []
                     | B.singleton '\\' == t             = []
                     | B.pack "\\%" `B.isPrefixOf` t     = findSimpleCommands $ B.drop 2 t
                     | B.singleton '%' `B.isPrefixOf` t  = findSimpleCommands $ safeTail $ B.dropWhile (/= '\n') t
                     | B.singleton '\\' `B.isPrefixOf` t = (command, param) : findSimpleCommands  rest
                     | otherwise                         = findSimpleCommands $ B.tail t
 where  (command,optParamRest) = B.span isAlpha (B.tail t)
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
findSimpleCommands ('\\':rest1) | null rest5                                    = [(command, "")]
                                | n == '{'                                      = (command,param) :findSimpleCommands rest3
                                | n == '[' && (length rest3') > 2 && m == '{'   = (command,param2):findSimpleCommands rest4
                                | otherwise                                     = (command,""):findSimpleCommands rest5
--                              | command == "printindex"                       = ("printindex", ""):findSimpleCommands (n:rest2)
--                              | otherwise                                     = findSimpleCommands (n:rest2)
        where (command,rest5)   = span isAlpha rest1
              (n:rest2)         = rest5
              (param,rest3)     = span (/='}') rest2
              (_,rest3')        = span (/=']') rest2
              (_:m:rest3'')     = rest3'
              (param2,rest4)    = span (/='}') rest3''
findSimpleCommands (_:rest)                                                     = findSimpleCommands rest             
-}

depCmds = [
        (B.pack "input",["tex","part.tex"]),
        (B.pack "include",["tex","part.tex"]),
        (B.pack "bibliography",["bib"]),
        (B.pack "usepackage",["sty"]),
        (B.pack "includegraphics",["","png"] )
        ]
depsTex wi tex =
        let file = smContent tex
            commands = findSimpleCommands file
            candits = catMaybes $  map (\(c,f) -> case lookup c depCmds of 
                                                        Just exts -> Just $ (B.unpack f,exts)
                                                        Nothing -> Nothing          ) commands
            files = filter (/=tex) $ catMaybes $ map find candits
        in files
        -- FIXME this for file in subdirectories!
        --              dir <- dirTrail (pagename tex),
 where  find (candit,exts) = do
                page <- lookupPage (PageName candit) (sitemap wi)
                guard $ smType page `elem` exts ||
                        ""          `elem` exts      --If any is allowed
                return page

texInclCmds = [B.pack "input", B.pack "include"]
prepareStripped :: PageInfo -> FileProducer ()
prepareStripped tex = do
        let file = smContent tex
        let dir = takeDirectory (pagename tex)
        let commands = findSimpleCommands file
            candits = map (dir </>) $ map B.unpack $ map snd $ filter (\(c,f) -> c `elem` texInclCmds) commands
        wi <- getWi
        let todo = mapMaybe (find wi) candits
        mapM_ snd todo
        mapM_ (prepareStripped . fst) todo
 where  find wi candit = do
                file <- lookupPage (PageName (dropExtensions candit)) (sitemap wi)
                method <- lookup (smType file) methods
                return $ (file,  method file (pageOutput file "tex"))
{- where  find candit = liftM listToMaybe $ filterM (doesFileExist.fst) [ 
                        (dir </> candit <.> suf, m (dir </> candit <.> suf) (candit <.> ".tex")) |
                                dir <- dirTrail (fileRelative tex),
                                (suf,m) <- methods
                        ] -}
        methods = [ ("part.tex", copy), ("tex",strip) ]
        --copy f t =  putStrLn ("copying   " ++ f ++ " to " ++ t) >> copyFile f t
        copy f t = liftIO $ copyFile (pageInput f) t
        --strip f t = putStrLn ("stripping " ++ f ++ " to " ++ t) >> ((writeFileSafe t). strip' =<< readFile f)
        strip f t = liftIO $ writeFileSafe t  stripped
         where 
                file = smContent f
                stripped = chaptertitle $ mainPart file 
                title = cleanupTitle $ fromMaybe (B.pack "No Title") $ lookup (B.pack "title") $ findSimpleCommands file
                chaptertitle = replaceBS (B.pack "\\maketitle") (B.pack "\\chapter{" `B.append` title `B.append` B.pack "}")
                mainPart =      B.unlines .
                                takeWhile (not. B.isPrefixOf (B.pack "\\end{document}")) .
                                stail .
                                dropWhile (not. B.isPrefixOf (B.pack "\\begin{document}")) .
                                B.lines
                stail [] = []
                stail l  = tail l

-- Todo: Replace all, find more stuff
cleanupTitle = replaceBS (B.pack "\\\\") (B.pack " - ")

hasCommand command tex = command  `elem` findSimpleCommands (smContent tex)

usesPST = hasCommand (B.pack "usepackage", B.pack "pst-pdf")
usesBibtex tex = B.pack "bibliography" `elem` map fst (findSimpleCommands (smContent tex))
usesIndex = hasCommand (B.pack "printindex", B.empty)
usesGlossaries = hasCommand (B.pack "printglossaries", B.empty)


procTex :: FileProcessor
procTex tex = do
        let htmlFile = pageOutput tex "html"
            pdfFile  = pageOutput tex "pdf"
            pngFile  = pageOutput tex "png"
            metaDataFile = pageOutput tex "metadata"
        --depRes <- needUpdates [htmlFile,pdfFile,pngFile] =<< findDeps tex
        --let up2date = isUpToDate depRes
        --liftIO $ showState (pagename tex) depRes
        return [ ( [htmlFile, pdfFile, pngFile, metaDataFile], do
                
                ok <- genPDF tex 
                metaData <- if ok then do
                        pdfInfo <- liftIO $ getPDFInfo pdfFile 
                        splitPDF pdfFile pdfInfo
                        genMetaData tex (Just pdfInfo)
                 else 
                        genMetaData tex Nothing
                saveMetaData tex metaData
                genHTML tex metaData
                )]

genMetaData :: PageInfo -> Maybe PDFData -> FileProducer MetaData
genMetaData tex mPDF = do
    wi <- getWi
    lc <- liftIO $ getGitLastChange (pageOutput tex "tex")
    return $ MetaData title lecturer semester state index lc mPDF
  where
    title = cleanupTitle $ fromMaybe (B.pack (pagename tex)) $ lookup (B.pack "title") $ findSimpleCommands $ smContent tex
    lecturer = lookup (B.pack "lecturer") $ findSimpleCommands $ smContent tex
    semester = lookup (B.pack "semester") $ findSimpleCommands $ smContent tex
    state = lookup (B.pack "scriptstate") $ findSimpleCommands $ smContent tex
    index = getIndex tex

saveMetaData :: PageInfo -> MetaData -> FileProducer ()
saveMetaData tex md = do
    let outfile = pageOutput tex "metadata"
    liftIO $ writeFile outfile $ show md
    

genPDF :: PageInfo -> FileProducer (Bool)
genPDF tex =  do 
        prepareStripped tex
        err <- liftIO $ whileOk =<< runLatex
        case err of
                ExitFailure _ -> liftIO $ putStrLn (pagename tex ++ ": LaTeX failed ("++show err ++")")
                ExitSuccess   -> return ()
        return $ err == ExitSuccess
  where realsource = fileRelative tex
        source     = takeFileName (pageOutput tex "tex")
        --realbasename = dropExtensions realsource
        output   = pageOutput tex "output"
        outDir   = takeDirectory (pagename tex)
        pdffile  = pageOutput tex "pdf"
        runLatex = do

        home <- getEnv "HOME"

        let env = [ ("TEXINPUTS",".:" ++ concatMap (++":") (dirTrail realsource)) -- colon to append, not override, default
                  , ("BIBINPUTS",".:" ++ concatMap (++":") (dirTrail realsource)) -- colon to append, not override, default
                  , ("HOME", home)
                  , ("PATH", "/usr/local/bin:/usr/bin:/bin")]
        
        let runit dir c a = do
                --let output = realbasename ++ ".output"
                appendFileSafe output $ "\nRunning "++c++" "++(concat (intersperse " " a))++
                                    " in an env with "++(show (length env))++" entries:\n"
                readNull <- openFile "/dev/null" ReadMode
                writeLog <- openFileSafe output AppendMode
                err <- inDir dir $
                        runProcess c a Nothing (Just env) (Just readNull) (Just writeLog) (Just writeLog) >>=
                        waitForProcess
                appendFile output $ "Result: "++(show err)++"\n"
                return err


        let clearOutput = do
                safeRemoveFile $ output
                return ExitSuccess

        let copyInput = do
                inDir outDir (copyFile realsource source)

        let pstqueue = if usesPST tex
                        then [
                                runit outDir "/usr/bin/latex" [ source ],
                                runit ""     "/usr/bin/dvips" [ pageOutput tex "dvi", "-o",
                                                                pageVariant tex "pics.ps" ],
                                runit ""     "/usr/bin/ps2pdf" [ pageVariant tex "pics.ps" ]
                        ]
                        else []
        
        let indexqueue = if usesIndex tex
                        then [
                                runit ""     "/usr/bin/makeindex" [ pageOutput tex "idx" ]
                        ]
                        else []
        let bibtexqueue = if usesBibtex tex
                        then [
                                runit ""     "/usr/bin/bibtex" [ pageOutput tex "aux" ]
                        ]
                        else []
        let glossariesqueue = if usesGlossaries tex
                        then [
                                runit ""     "/usr/bin/makeglossaries" [ pageOutput tex "glo" ]
                        ]
                        else []
        
        let latexrun =          runit outDir "/usr/bin/pdflatex" [source]

        let pngrun =    [
                                runit ""     "/usr/bin/convert" [ "-verbose", pdffile ++ "[0]", "-resize", "70x", pageOutput tex "png" ] 
                        ,       runit ""    "/usr/bin/optipng" [ "-verbose", pageOutput tex "png" ]
                        ]
        
        return $ [clearOutput] ++
                 pstqueue ++
                 [latexrun] ++
                 indexqueue ++
                 bibtexqueue ++
                 glossariesqueue ++
                 replicate 2 latexrun ++
                 pngrun

genHTML :: PageInfo -> MetaData -> FileProducer ()
genHTML tex md@MetaData{..} = do 
        let title = mdTitle
            titleline = [Header 1 (B.pack "Latex File: " `B.append` title)]
        writeHtmlPage target tex (B.unpack title) $
            titleline ++ [LIElem (LectureInfo tex (Just md))]
  where target  = pageOutput tex "html"


findspans :: header -> (line -> Maybe header) -> [line] -> [((Int, Int), header)]
findspans _      _       []   = []
findspans first extract list = findspans' first (map extract list) 1 0
  where findspans' current []            a b = [((a,b), current)]
        findspans' current (Just new:xs) a b =  ((a,b), current) : findspans' new     xs (b+1) (b+1)
        findspans' current (Nothing :xs) a b =                     findspans' current xs  a    (b+1)

getIndex :: PageInfo -> [TexIndex]
getIndex tex = map toTexIndex . extract . map uncomment . B.lines $ smContent tex
  where extract = findspans (B.pack "Preamble") extract_chapter 
        extract_chapter line = listToMaybe $ do (command,param) <- findSimpleCommands line
                                                guard $ command == B.pack "chapter"
                                                return param
        toTexIndex ((a,b),t) = TexIndex t a b

formatIndex :: PageInfo -> [TexIndex] -> [DocElement]
formatIndex tex = (Header 2 (B.pack "Index-Preview"):) . (:[]) . ItemList . map format

  where format (TexIndex{..}) = [
            Text (tiTitle `B.append` B.pack " "),
            LinkElem (PlainLink (B.pack $ editLinkLines tex (tiPageFrom) (tiPageTo)) (B.pack "(bearbeiten)")
            )]

getTitle tex = fromMaybe (B.pack (pagename tex)) . lookup (B.pack "title") . findSimpleCommands  

