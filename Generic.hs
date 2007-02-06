module Generic ( procGeneric, procCopyGen, procCopy ) where

import WikiData
import Common
import HtmlStyle
import Dependencies

import Char
import Maybe
import Monad
import System.Directory

procGeneric isBinary file = do 
	let htmlFile  = (pagename file) ++ ".html"
	depRes <- needUpdate htmlFile [file]
	let up2date = isUpToDate depRes
	liftIO $ showState (pagename file) depRes
	unless up2date $ do
		content <- if isNothing isBinary then do
			source <- liftIO $ readFile file
			return $ if isReadable source then pre source else binary
		  else	if fromJust isBinary then return binary
			else liftIO $ pre `liftM` readFile file
		writeHtmlPage htmlFile (pagename file) (pagename file) content 
	producedFile htmlFile
  where	isReadable = not.(any (=='\0'))
  	pre source = [Header 1 file,
	              Paragraph [LinkElem (DLLink link)],
	  	      Header 2 "Source",
		      PreFormat source]
  	binary   = [Header 1 file,
	            Paragraph [LinkElem (DLLink link)]]
	link = backDir (pagename file) ++ file

procCopyGen file = procCopy file >> procGeneric Nothing file 

procCopy file = liftIO (copyFile file (filename file)) >> producedFile (filename file)

