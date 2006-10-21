{-

 Latexki © 2006 Joachim Breitner

-}

import System
import System.IO
import Directory
import Monad
import List

import Common
import Wiki
import Latex
import Generic
import SVN

pipes :: String -> ( [String], FileProcessor )
pipes "tex"   = (["html","pdf","log","output"],  procTex  )
pipes "latex" = pipes "tex"
pipes ""      = (["html"], procWiki    )
pipes "css"   = (["html","css"], procCopyGen ) 
pipes _       = (["html"], procGeneric ) 

deps :: String -> DepCalculator
deps "tex"    = texDeps 
deps "latex"  = deps "tex"
deps ""       = wikiDeps
deps _        = const . return . (:[]) . FileDep

actions file = do 
	let  (basename, ext) = splitWikiPath file
	     withExt e       = basename++"."++e
	     (exts, action)  = pipes ext
	     act             = (action file, map withExt exts)
	     sitemapEntry    = (basename, ext, exts)
             myDeps          =  deps ext file
        return $ (sitemapEntry, (act, myDeps))

-- file1 depends upon file2
-- Todo, only when new files are there. Probably hard
needsUpdate debugLn file1 FileList          = do
	debugLn $ "  "++file1++" updated because of possible new files"
	return True 
needsUpdate debugLn file1 RepositoryChanges = do
	debugLn $ "  "++file1++" updated because of Repositorychanges"
	return True 
needsUpdate debugLn file1 (FileDep file2)   = do 
	ex <- doesFileExist file1
	ex2 <- doesFileExist file2
	needs_update <-
	  if not ex2 then do
		debugLn $ "WARNING: Dependency "++file2++" does not exist"
		return False
	  else if ex then do
		date1 <- getModificationTime file1
		date2 <- getModificationTime file2
		return $ date1 < date2
	    else return True
	when needs_update $ debugLn $ "  "++file1++" update because of "++file2
	return needs_update

anyM  cond list = mapM cond list >>= return.or
anyM2 cond list1 list2 = mapM (uncurry cond) [(a,b) | a <- list1 , b <- list2] >>= return.or

readConfig = do 
	exists <- doesFileExist file
	if exists then
		return.(map extract).(filter (not.isComment)).lines =<< readFile file
	  else
		return []
  where file = datadir ++ "latexki-main.wiki-conf"		
  	isComment ('#':_) = True
	isComment l  | all (`elem` whitespace) l = True
	             | otherwise            = False
        extract l = let (p1,':':p2) = span (/=':') l in (p1, dropWhile (`elem` whitespace) p2)
	whitespace = " \t"
		

main = do
  (opts, [repos, outdir]) <- partition ("-" `isPrefixOf`) `liftM` getArgs
  exists <- doesDirectoryExist outdir
  unless exists $ ioError $ userError $ "Outdir "++outdir++" does not exist"
  setCurrentDirectory outdir

  terminal <- hIsTerminalDevice stdout
  logfile <- openFile logfilename WriteMode
  let debug str = do	if terminal then hPutStr stdout str else return ()
  			hPutStr logfile str
  let debugLn = debug . (++"\n")

  exported <- doesDirectoryExist (datadir++".svn")
  if exported then if "-n" `notElem` opts then updateSVN repos
	                                  else return ()
              else                             coSVN repos

  inputfiles <- sort `liftM` recursiveFiles datadir
  --inputfiles <- (filter (not.null.basename) . sort) `liftM` recursiveFiles datadir

  debug "Reading Configuration..."
  config <- readConfig
  debugLn "done."

  debug "Generating Sitemap..."
  (sm, todo'') <- unzip `liftM` mapM actions inputfiles
  debugLn $ (show $ length sm) ++ " base files."

  debug "Generating Recent Changes.."
  rc <- if "-n" `notElem` opts then getSVNRecentChanges repos else return []
  debugLn "Done."
  
  let wi = WikiInfo { sitemap = sm , wikiConfig = config, recentChanges = rc, debug=debug, debugLn=debugLn}

  debug "Getting Dependencies..."
  todo' <- mapM ( \(act,dep) -> dep wi >>= return.((,) act) ) todo'' 
  debugLn "Done."

  debugLn "Checking for up-to-dateness..."
  todo <- filterM (\((_,t),d) -> anyM2 (needsUpdate debugLn) t d) todo' >>= return.(map fst)
  debugLn $ (show $ length todo)++" left to do."

  debugLn "Generating files... "
  mapM_ (\(a,f) -> debug ((concat(intersperse ", " f))++"...") >> a wi >> debugLn ".") todo
  debugLn "Done."

  debugLn "Cleaning up..."
  foundOutputs <- recursiveFiles "./"
  let expectedOutputs = map ("./"++) $ outputs wi
      systemFiles = [logfilename]
      delete = filter (`notElem` expectedOutputs) $
      		filter (`notElem` systemFiles) $
		filter (not . isPrefixOf datadir ) $
      		foundOutputs
  debug $ "Deleting "++(show (length delete) ) ++ " old or temporary files.. "
  mapM_ (\f -> debugLn ("Deleting old or temporary file  "++f)  >> removeFile f) delete
  --mapM_ removeFile delete
  debugLn "Done."
  hClose logfile


  return ()


