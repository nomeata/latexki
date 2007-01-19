{-

 Latexki © 2006 Joachim Breitner

-}

import System
import System.IO
import System.Posix.IO
import Directory
import Monad
import List

import Common
import Wiki
import Latex
import Generic
import ImageFile
import SVN
import DepMapT

import Data.Map ((!))

pipes :: String -> ( [String], FileProcessor )
pipes "tex"   = (["html","pdf","png"],  procTex  )
pipes "latex" = pipes "tex"
pipes ""      = (["html","pdf"], procWiki    )
pipes "css"   = (["html","css"], procCopyGen ) 
pipes "png"   = (["html","pdf"], procImage ) 
pipes "jpg"   = (["html","pdf"], procImage ) 
pipes "eps"   = (["html"], procGeneric (Just True))
pipes _       = (["html"], procGeneric Nothing) 

deps :: String -> DepCalculator
deps "tex"    = texDeps 
deps "latex"  = deps "tex"
deps ""       = wikiDeps
deps _        = const . return . (:[]) . fileDep

anyDep wi file = deps (snd (splitWikiPath file)) file wi

actions file = do 
	let  (basename, ext) = splitWikiPath file
	     withExt e       = basename++"."++e
	     (exts, action)  = pipes ext
	     act             = (action file, map withExt exts)
	     sitemapEntry    = (basename, ext, exts)
--             myDeps          =  deps ext file
        return $ (sitemapEntry, (act, file))

-- file1 depends upon file2
-- Todo, only when new files are there. Probably hard
needsUpdate putStrLn file1 (Special FileList) = do
	putStrLn $ "  "++file1++" updated because of possible new files"
	return True 
needsUpdate putStrLn file1 (Special RepositoryChanges) = do
	putStrLn $ "  "++file1++" updated because of Repositorychanges"
	return True 
needsUpdate putStrLn file1 (Dep file2)   = do 
	ex <- doesFileExist file1
	ex2 <- doesFileExist file2
	needs_update <-
	  if not ex2 then do
		putStrLn $ "WARNING: Dependency "++file2++" does not exist"
		return False
	  else if ex then do
		date1 <- getModificationTime file1
		date2 <- getModificationTime file2
		return $ date1 < date2
	    else return True
	when needs_update $ putStrLn $ "  "++file1++" update because of "++file2
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
  if not terminal then do
  	-- if we try to run this program on windows, this might make problems
	  logfile   <- openFile logfilename WriteMode
	  logfileFd <- handleToFd logfile
	  dupTo logfileFd stdError
	  dupTo logfileFd stdOutput
	  return ()
	else return ()		 

  exported <- doesDirectoryExist (datadir++".svn")
  if exported then if "-n" `notElem` opts then updateSVN repos
	                                  else return ()
              else                             coSVN repos

  inputfiles <- (filter (not . isPrefixOf (drop 2 datadir) . pagename) . sort) `liftM` recursiveFiles datadir
  --inputfiles <- (filter (not.null.basename) . sort) `liftM` recursiveFiles datadir

  putStr "Reading Configuration..."
  config <- readConfig
  putStrLn "done."

  putStr "Generating Sitemap..."
  (sm, todo'') <- unzip `liftM` mapM actions inputfiles
  putStrLn $ (show $ length sm) ++ " base files."


  putStr "Generating Recent Changes.."
  rc <- if "-n" `notElem` opts then getSVNRecentChanges repos else return []
  putStrLn "Done."
  
  let wi = WikiInfo { sitemap = sm , wikiConfig = config, recentChanges = rc}

  putStr "Getting Dependencies..."
  depMap <- genDepMap (map snd todo'') (anyDep wi) 
  let todo' = map ( \(act,file) -> (act, depMap ! file)) todo'' 
  putStrLn "Done."

  putStrLn "Checking for up-to-dateness..."
  todo <- filterM (\((_,t),d) -> anyM2 (needsUpdate putStrLn) t d) todo' >>= return.(map fst)
  putStrLn $ (show $ length todo)++" left to do."

  putStrLn "Generating files... "
  mapM_ (\(a,f) -> putStr ((concat(intersperse ", " f))++"...") >> a wi >> putStrLn ".") todo
  putStrLn "Done."

  putStrLn "Cleaning up..."
  foundOutputs <- recursiveFiles "./"
  let expectedOutputs = map ("./"++) $ outputs wi
      systemFiles = [logfilename]
      putStrExts   = [".log",".output"]
      delete =  filter (\f -> not $ any (\e -> e `isSuffixOf` f) putStrExts) $
      		filter (`notElem` expectedOutputs) $
      		filter (`notElem` systemFiles) $
		filter (not . isPrefixOf datadir ) $
      		foundOutputs
  putStrLn $ "Deleting "++(show (length delete) ) ++ " old or temporary files:\n" ++ concat (intersperse ", " delete)
  --mapM_ (\f -> putStrLn ("Deleting old or temporary file  "++f)  >> removeFile f) delete
  mapM_ removeFile delete
  putStrLn "Done."

  return ()


