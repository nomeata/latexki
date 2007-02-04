module Generic ( procGeneric, procCopyGen, procCopy ) where

import WikiData
import Common
import HtmlStyle
import Dependencies

import Char
import Maybe
import Monad
import System.Directory

procGeneric isBinary file wi = do 
	let htmlFile  = (pagename file) ++ ".html"
	depRes <- liftIO $ needUpdate htmlFile [file]
	let up2date = isUpToDate depRes
	liftIO $ showState (pagename file) depRes
	unless up2date $ do
		content <- if isNothing isBinary then do
			source <- liftIO $ readFile file
			return $ if isReadable source then pre source else binary
		  else	if fromJust isBinary then return binary
			else liftIO $ pre `liftM` readFile file
		liftIO $ writeHtmlPage wi htmlFile (pagename file) (pagename file) content 
	producedFile htmlFile
  where	isReadable = not.(any (=='\0'))
  	pre source = [Header 1 file,
	              Paragraph [LinkElem (DLLink link)],
	  	      Header 2 "Source",
		      PreFormat source]
  	binary   = [Header 1 file,
	            Paragraph [LinkElem (DLLink link)]]
	link = backDir (pagename file) ++ file

procCopyGen file wi = procCopy file wi >> procGeneric Nothing file wi

procCopy file wi = liftIO (copyFile file (filename file)) >> producedFile (filename file)

