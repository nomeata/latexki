{-

 Latexki © 2006 Joachim Breitner

-}

import System
import Directory
import Monad
import List

import Common
import Wiki
import Latex
import Generic
import SVN

directoryFiles dir = getDirectoryContents dir >>= return.(map (dir++)) >>= filterM (doesFileExist)

pipes :: String -> ( [String], FileProcessor )
pipes "tex"   = (["html","pdf","log"],  procTex  )
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
	let  (dir, basename, ext) = splitFilePath file
	     withExt e       = basename++"."++e
	     (exts, action)  = pipes ext
	     act             = (action file, map withExt exts)
	     sitemapEntry    = (basename, ext, exts)
             myDeps          =  deps ext file
        return $ (sitemapEntry, (act, myDeps))

-- file1 depends upon file2
-- Todo, only when new files are there. Probably hard
needsUpdate file1 FileList          = do
	putStrLn $ "  "++file1++" updated because of possible new files"
	return True 
needsUpdate file1 RepositoryChanges = do
	putStrLn $ "  "++file1++" updated because of Repositorychanges"
	return True 
needsUpdate file1 (FileDep file2)   = do 
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
  exported <- doesDirectoryExist (datadir++".svn")
  if exported then if "-n" `notElem` opts then updateSVN repos
	                                  else return ()
              else                             coSVN repos

  inputfiles <- (filter (not.null.basename) . sort) `liftM` directoryFiles datadir

  putStr "Reading Configuration..."
  config <- readConfig
  putStrLn "done."

  putStr "Generating Sitemap..."
  (sm, todo'') <- unzip `liftM` mapM actions inputfiles
  putStrLn $ (show $ length sm) ++ " base files."

  putStr "Generating Recent Changes.."
  rc <- getSVNRecentChanges repos
  putStrLn "Done."
  
  let wi = WikiInfo { sitemap = sm , wikiConfig = config, recentChanges = rc}

  putStr "Getting Dependencies..."
  todo' <- mapM ( \(act,dep) -> dep wi >>= return.((,) act) ) todo'' 
  putStrLn "Done."

  putStrLn "Checking for up-to-dateness..."
  todo <- filterM (\((_,t),d) -> anyM2 needsUpdate t d) todo' >>= return.(map fst)
  putStrLn $ (show $ length todo)++" left to do."

  putStrLn "Generating files... "
  mapM_ (\(a,f) -> putStr ((concat(intersperse ", " f))++"...") >> a wi >> putStrLn ".") todo
  putStrLn "Done."

  putStrLn "Cleaning up..."
  foundOutputs <- directoryFiles "./"
  let expectedOutputs = outputs wi
      delete = filter (`notElem` expectedOutputs) $ map filename foundOutputs
  putStr $ "Deleting "++(show (length delete) ) ++ " old or temporary files.. "
  --mapM_ (\f -> putStrLn ("Deleting old or temporary file  "++f)  >> removeFile f) delete
  mapM_ removeFile delete
  putStrLn "Done."


  return ()


