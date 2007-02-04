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

actions file = do 
	let  (basename, ext)  = splitWikiPath file
	     withExt e        = basename++"."++e
	     (exts, producer) = pipes ext
	     sitemapEntry     = (basename, ext, exts)
        return $ (sitemapEntry, producer file)


{-
anyM  cond list = mapM cond list >>= return.or
anyM2 cond list1 list2 = mapM (uncurry cond) [(a,b) | a <- list1 , b <- list2] >>= return.or
-}

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
  (opts, [repos, outdir]) <- partition ("-" `isPrefixOf`) `fmap` getArgs
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

  inputfiles <- (filter (not . isPrefixOf (drop 2 datadir) . pagename) . sort) `fmap` recursiveFiles datadir

  putStr "Reading Configuration..."
  config <- readConfig
  putStrLn "done."

  putStr "Generating Sitemap..."
  (sm, todo) <- unzip `fmap` mapM actions inputfiles
  putStrLn $ (show $ length sm) ++ " base files."


  putStr "Generating Recent Changes.."
  rc <- if "-n" `notElem` opts then getSVNRecentChanges repos else return []
  putStrLn "Done."
  
  let wi = WikiInfo { sitemap = sm , wikiConfig = config, recentChanges = rc}
  
  -- This is where all the action happens
  producedFiles <- runFileProducers $ map ($wi) todo

  --putStrLn $ "Produced: "++show producedFiles

  putStrLn "Cleaning up..."
  foundOutputs <- recursiveFiles "./"
  let --expectedOutputs = map ("./"++) $ outputs wi
      expectedOutputs = map ("./"++) producedFiles
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


