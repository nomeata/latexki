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

pipes :: String -> ( [String], FileProcessor )
pipes "tex"   = (["pdf","log"],  procTex  )
pipes "latex" = pipes "tex"
pipes ""      = (["html"], procWiki    )
pipes _       = (["html"], procGeneric ) 

deps :: String -> DepCalculator
deps "tex"    = texDeps 
deps "latex"  = deps "tex"
deps _        = \file -> const (return [file])

actions file = do 
	let  (dir, basename, ext) = splitFilePath file
	     withExt e       = basename++"."++e
	     (exts, action)  = pipes ext
	     act             = (action file, map withExt exts)
	     sitemapEntry    = (basename, ext, exts)
             myDeps          =  deps ext file
        return $ (sitemapEntry, (act, myDeps))

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

  putStr "Generating Sitemap..."
  (sm, todo'') <- return.unzip =<< mapM actions inputfiles  
  putStrLn $ (show $ length sm )++" base files."
  
  let wi = WikiInfo { sitemap = sm }

  putStr "Getting Dependencies..."
  todo' <- mapM ( \(act,dep) -> dep wi >>= return.((,) act) ) todo'' 
  putStrLn "Done."

  putStrLn "Checking for up-to-dateness..."
  todo <- filterM (\((_,t),d) -> anyM2 needsUpdate t d) todo' >>= return.(map fst)
  putStrLn $ (show $ length todo)++" left to do."

  putStrLn "Generating files..."
  mapM_ (\(a,f) -> putStrLn ((concat(intersperse ", " f))++"...") >> a wi) todo
  putStrLn "Done."

  putStrLn "Cleaning up..."
  foundOutputs <- directoryFiles "./"
  let expectedOutputs = outputs wi
      delete = filter (`notElem` expectedOutputs) $ map filename foundOutputs
  mapM_ (\f -> putStrLn ("Deleting "++f)  >> removeFile f) delete
  putStrLn "Done."


  return ()


