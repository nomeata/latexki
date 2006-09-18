{-

 Latexki © 2006 Joachim Breitner

-}

import System
import Directory
import Monad
import List

splitFileExt p =
 	  case break (== '.') fname of
 	        (suf@(_:_),_:pre) -> (reverse (pre++path), reverse suf)
 	        _                 -> (p, [])
 	  where
 	    (fname,path) = break (=='/') (reverse p)

produces "tex"   = ["html", "pdf"]
produces "latex" = produces "tex"
produces "wiki"  = ["html"]
produces "png"   = ["html"]
produces "jpg"   = ["html"]
produces _       = []

directoryFiles dir = getDirectoryContents dir  >>= return.(map ((dir++).("/"++))) >>=  filterM (doesFileExist)

texDeps file = return []
noDeps  file = return []

tex2pdf  tex pdf = return ()
tex2html tex html = return ()
wiki2html tex html = return ()
generic2html file html = return ()

pipes :: String -> [ ( String, FilePath -> IO [FilePath], FilePath -> FilePath -> IO () ) ]
pipes "tex" =  [("pdf",  noDeps, tex2pdf      ),
                ("html", noDeps, tex2html     ) ]
pipes "wiki" = [("html", noDeps, wiki2html    ) ]
pipes "_"    = [("html", noDeps, generic2html ) ]

actions file = do 
	let  (basename, ext) = splitFileExt file
	     withExt e       = basename++"."++e
	     (acts, depIOs)  = unzip $ map (\(to, deps, action) ->((action file, (withExt to)), deps file)) $ pipes ext
        deps <- sequence depIOs
        return $ zip acts (map (file:) deps)

needsUpdate file1 file2 = do 
	ex <- doesFileExist file1
	if ex then do
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
  let datadir = "./data"

  exported <- doesDirectoryExist (datadir++"/.svn")
  if exported then
  	system ("(cd "++datadir++"; svn update)")
    else 
  	system ("svn checkout "++repos++" "++datadir)

  inputfiles <- directoryFiles datadir
  old_outputs <- directoryFiles "."

  putStr "Getting dependencies..."
  todo' <- mapM actions inputfiles >>= return.concat
  putStrLn $ (show $ length todo')++" of these."

  putStr "Checking for up-to-dateness..."
  todo <- filterM (\((_,t),d) -> anyM (t `needsUpdate`) d) todo' >>= return.(map fst)
  putStrLn $ (show $ length todo)++" left to do."

  putStrLn "Generating files..."
  mapM_ (\(a,f) -> putStrLn (f++"...") >> a f) todo
  putStrLn "Done."


  return ()


