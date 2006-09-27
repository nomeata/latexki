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

directoryFiles dir = getDirectoryContents dir >>= return.(map (dir++)) >>= filterM (doesFileExist)

pipes :: String -> ( [String], WikiInfo -> FilePath -> IO () )
pipes "tex"   = (["pdf","log"],  procTex  )
pipes "latex" = pipes "tex"
pipes ""      = (["html"], procWiki    )
pipes _       = (["html"], procGeneric ) 

deps wi "tex"   = texDeps wi
deps wi "latex" = deps wi "tex"
deps wi _     = return.(const [])

actions wi file = do 
	let  (dir, basename, ext) = splitFilePath file
	     withExt e       = basename++"."++e
	     (exts, action)  = pipes ext
	     act             = (action wi file, map withExt exts)
	     sitemapEntry    = (basename, ext, exts)
        myDeps <- deps wi ext file
        return $ (sitemapEntry, act, file:myDeps)

-- file1 depends upon file2
needsUpdate file1 file2 = do 
	putStrLn $ "  "++file1++" depends on "++file2
	ex <- doesFileExist file1
	ex2 <- doesFileExist file2
	if not ex2 then do
		putStrLn $ "WARNING: Dependency "++file2++" does not exist"
		return False
	  else if ex then do
		date1 <- getModificationTime file1
		date2 <- getModificationTime file2
		return $ date1 < date2
	    else return True

anyM  cond list = mapM cond list >>= return.or
anyM2 cond list1 list2 = mapM (uncurry cond) [(a,b) | a <- list1 , b <- list2] >>= return.or

main = do
  [repos,outdir] <- getArgs

  exists <- doesDirectoryExist outdir
  unless exists $ ioError $ userError $ "Outdir "++outdir++" does not exist"
  setCurrentDirectory outdir
  exported <- doesDirectoryExist (datadir++".svn")
  if exported then
  	system ("(cd "++datadir++"; svn update)")
    else 
  	system ("svn checkout "++repos++" "++datadir)

  inputfiles <- directoryFiles datadir
  let wi = WikiInfo { sitemap = [] }

  putStr "Generating Sitemap..."
  todo' <- mapM (actions wi) inputfiles 
  putStrLn $ (show $ length todo')++" of these."
  
  let wi = WikiInfo { sitemap = map triple1 todo' }

  putStr "Checking for up-to-dateness..."
  todo <- filterM (\(_,(_,t),d) -> anyM2 needsUpdate t d) todo' >>= return.(map triple2)
  putStrLn $ (show $ length todo)++" left to do."

  putStrLn "Generating files..."
  mapM_ (\(a,f) -> putStrLn ((concat(intersperse ", " f))++"...") >> a) todo
  putStrLn "Done."

  putStrLn "Cleaning up..."
  foundOutputs <- directoryFiles "./"
  let expectedOutputs = outputs wi
      delete = filter (`notElem` expectedOutputs) $ map filename foundOutputs
  mapM_ (\f -> putStrLn ("Deleting "++f)  >> removeFile f) delete
  putStrLn "Done."


  return ()


