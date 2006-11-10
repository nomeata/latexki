module Generic ( procGeneric, procCopyGen, procCopy ) where

import WikiData
import Common
import HtmlStyle

import Char
import Maybe
import Monad
import System.Directory

procImage image wi = do 
	let target  = (pagename image) ++ ".html"
	writeFileSafe target $ htmlPage wi (pagename image) (pagename image) content 
  where	content  = [Header 1 (pagename image),
	            Paragraph [Image link (pagename image)],
	            Paragraph [LinkElem (DLLink link)]]
	link = backDir (pagename image) ++ image		 



procGeneric isBinary file wi = do 
	content <- 
		if isNothing isBinary then do
			source <- readFile file
			return $ if isReadable source then pre source else binary
		  else	if fromJust isBinary then return binary
			else pre `liftM` readFile file
	writeHtmlPage wi target (pagename file) (pagename file) content 
  where	isReadable = not.(any (=='\0'))
	target  = (pagename file) ++ ".html"
  	pre source = [Header 1 file,
	              Paragraph [LinkElem (DLLink link)],
	  	      Header 2 "Source",
		      PreFormat source]
  	binary   = [Header 1 file,
	            Paragraph [LinkElem (DLLink link)]]
	link = backDir (pagename file) ++ file

procCopyGen file wi = procCopy file wi >> procGeneric Nothing file wi

procCopy file wi = copyFile file (filename file)

