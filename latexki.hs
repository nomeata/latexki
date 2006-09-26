{-

 Latexki © 2006 Joachim Breitner

-}

import System
import Directory
import Monad
import List

import FilePath

import Common
import Wiki
import Latex
import Generic

directoryFiles dir = getDirectoryContents dir >>= return.(map (dir++)) >>= filterM (doesFileExist)

pipes :: String -> [ ( String, WikiInfo -> FilePath -> FilePath -> IO () ) ]
pipes "tex"   =  [("pdf",  tex2pdf      )]
--                  ("html", tex2html     ) ]
pipes "latex" = pipes "tex"
pipes ""      = [("html", wiki2html    ) ]
pipes _       = [("html", generic2html ) ]

deps wi "tex"   = texDeps wi
deps wi "latex" = deps wi "tex"
deps wi _     = return.(const [])

actions wi file = do 
	let  (dir, basename, ext) = splitFilePath file
	     withExt e       = basename++"."++e
	     acts            = map (\(to, action) ->(action wi file, (withExt to))) $ pipes ext
        myDeps <- deps wi ext file
        return $ map (flip (,) (file:myDeps)) acts

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

anyM cond list = mapM cond list >>= return.or

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
  old_outputs <- directoryFiles "./"
  let wi = WikiInfo { basenames = map basename inputfiles }

  putStr "Getting dependencies..."
  todo' <- mapM (actions wi) inputfiles >>= return.concat
  putStrLn $ (show $ length todo')++" of these."

  putStr "Checking for up-to-dateness..."
  todo <- filterM (\((_,t),d) -> anyM (t `needsUpdate`) d) todo' >>= return.(map fst)
  putStrLn $ (show $ length todo)++" left to do."

  putStrLn "Generating files..."
  mapM_ (\(a,f) -> putStrLn (f++"...") >> a f) todo
  putStrLn "Done."

  putStrLn "Cleaning up..."
  let delete = filter ((`notElem` (basenames wi)).basename) old_outputs
  mapM_ (\f -> putStrLn ("Deleting "++f)  >> removeFile f) delete
  putStrLn "Done."


  return ()


