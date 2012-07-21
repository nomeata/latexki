{-

 Latexki © 2006, 2007 Joachim Breitner

-}

import System.IO
import System.Posix.IO
import System.FilePath
import System.Directory
import System.Environment
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Graph

import Dependencies
import WikiData
import Common
import Wiki
import Latex
import Generic
import ImageFile
import SVN
import ReadDir

import Data.Map ((!))

run_producer :: PageInfo -> FileProducer [ ([FilePath], FileProducer () ) ]
run_producer page = producer (pageType page) page

producer :: String -> PageInfo -> FileProducer [ ([FilePath], FileProducer () ) ]
producer "tex"   = procTex 
producer "latex" = producer "tex"
producer ""      = procWiki
producer "css"   = procCopy
producer "js"    = procCopy
producer "png"   = procImage 
producer "jpg"   = procImage
producer "eps"   = procGeneric (Just True)
producer _       = procGeneric Nothing

run_deps wi page = deps (pageType page) wi page

deps :: String -> WikiInfo -> PageInfo -> [PageInfo]
deps "tex" = depsTex 
deps "part.tex" = depsTex 
deps "sty" = depsTex 
deps ""    = depsWiki
deps _     = \_ _ -> []

do_always wi page = always (pageType page) wi page

always "" = alwaysWiki
always _  = (\_ _ -> False)

{-
actions file = do 
	let  (basename, ext)  = splitWikiPath file
	     withExt e        = basename++"."++e
	     (exts, producer) = pipes ext
	     sitemapEntry     = (basename, ext, exts)
        return $ (sitemapEntry, producer file)
-}

{-
anyM  cond list = mapM cond list >>= return.or
anyM2 cond list1 list2 = mapM (uncurry cond) [(a,b) | a <- list1 , b <- list2] >>= return.or
-}

-- Whether a file is a derived file (with additional extension in between, e.g. bla.pdf → bla.1.pdf)
deriv `notDerived` files = not $ any (isStrip deriv) files
  where isStrip deriv file = takeExtension deriv == takeExtension file &&
                             dropExtensions deriv == dropExtensions file
			  
-- Transitive Hull
transHull :: (Ord a, Eq a) => [(a,S.Set a)]  -> [(a,S.Set a)]
transHull rel = foldl nextPlease [] rel
  where nextPlease :: (Ord a, Eq a) => [(a,S.Set a)] -> (a,S.Set a) -> [(a,S.Set a)]
	nextPlease done x@(c,d) = (c, newdeps) : done'
	  where newdeps = d `S.union` (S.unions $ S.toList $ S.map (fromMaybe S.empty . flip lookup done) d)
	        done'   = map (`trans` (c, newdeps)) done 
	(c,d) `trans` (c2,d2) = (c, d `S.union` (if c2 `S.member` d then d2 else S.empty))


readConfig = do 
	exists <- doesFileExist file
	if exists then
		return.(map extract).(filter (not.isComment)).lines =<< readFile file
	  else
		return []
  where file = datadir </> "latexki-main.wiki-conf"		
  	isComment ('#':_) = True
	isComment l  | all (`elem` whitespace) l = True
	             | otherwise            = False
        extract l = let (p1,':':p2) = span (/=':') l in (p1, dropWhile (`elem` whitespace) p2)
	whitespace = " \t"
		

main = do
  putStrLn "latexki starting..."
  (opts, [repos, outdir]) <- partition ("-" `isPrefixOf`) `fmap` getArgs
  exists <- doesDirectoryExist outdir
  unless exists $ ioError $ userError $ "Outdir "++outdir++" does not exist"
  setCurrentDirectory outdir

  -- Setting thigs up (e.g. loggin, svn updating)

  terminal <- hIsTerminalDevice stdout
  if not terminal then do
  	-- if we try to run this program on windows, this might make problems
	  logfile   <- openFile logfilename WriteMode
	  logfileFd <- handleToFd logfile
	  dupTo logfileFd stdError
	  dupTo logfileFd stdOutput
	  return ()
	else return ()		 
  hSetBuffering stdout NoBuffering

  exported <- doesDirectoryExist (datadir++".svn")
  if exported then if "-n" `notElem` opts then updateSVN repos
	                                  else return ()
              else                             coSVN repos

  putStr "Reading Configuration..."
  config <- readConfig
  putStrLn "done."

  putStr "Reading Directory..."
  inputfiles <- filter (not . isPrefixOf (normalise datadir) . deFileName) `liftM`
  		readDir datadir

--  (sm, todo) <- unzip `fmap` mapM actions inputfiles
  putStrLn $ (show $ length inputfiles) ++ " base files."


  putStr "Generating Recent Changes.."
  rc <- if "-n" `notElem` opts then getSVNRecentChanges repos else return []
  putStrLn "Done."
  
  putStr "Finding existing files.."
  foundOutputs <- filter (not . isPrefixOf (normalise datadir) . deFileName) `liftM`
  			readDir ""
  putStrLn "Done."

  let wi = WikiInfo {
  	sitemap = sort $ map de2sm inputfiles,
  	wikiConfig = config,
	recentChanges = rc,
	existingOutput = foundOutputs,
        repoPath = repos
	}

  putStr "Find out there is to do.."
  let depmap = map (fmap S.toList) $ transHull $ map (\p -> (p,S.fromList (run_deps wi p))) (sitemap wi)
  let always = filter (do_always wi) (sitemap wi)
  putStrLn "Done."

  putStr "Toplogically sorting pages.."
  --mapM_ print depmap
  let pages = flattenSCCs $ stronglyConnComp $ map (\page -> (page, page, fromMaybe [] (lookup page depmap))) (sitemap wi)
  putStrLn "Done."

  putStrLn "Generating files as needed.."
  -- This is where all the action happens
  producedFiles <- runFileProducer wi $ forM_ pages $ \page -> do 
	x <- return True
  	actions <- run_producer page	
	let force = page `elem` always
  	flip mapM_ actions $ \(outputs, action) -> do
		old <- anysOlder (page:fromMaybe [] (lookup page depmap)) outputs
		forM_ outputs producedFile 
		when (force || old) $ do
			liftIO $ putStrLn ("Generating outdated files: " ++ concat (intersperse ", " outputs))
			action

  -- Debug:
  --putStrLn $ "Produced: "++show producedFiles

  putStrLn "Cleaning up..."
  foundOutputs <- map deFileName `liftM` readDir ""
  let systemFiles = [logfilename]
      putStrExts   = [".log",".output"]
      delete =  filter (\f -> not $ any (takeExtension f ==) putStrExts) $
		filter (`notDerived` producedFiles) $ 
      		filter (`notElem` producedFiles) $
      		filter (`notElem` systemFiles) $
		filter (not . isPrefixOf datadir ) $
      		foundOutputs
  putStrLn $ "Deleting "++(show (length delete) ) ++ " old or temporary files:\n" ++ concat (intersperse ", " delete)
  --mapM_ (\f -> putStrLn ("Deleting old or temporary file  "++f)  >> removeFile f) delete
  mapM_ removeFile delete
  putStrLn "Done."

  return ()


