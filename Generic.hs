module Generic ( procGeneric, procCopyGen, procCopy ) where

import Common
import HtmlStyle
import Char
import System.Directory

procGeneric file wi = do 
	source <- readFile file
	let content = if isReadable source then pre source else binary
	    target  = (pagename file) ++ ".html"
	writeFileSafe target $ htmlPage wi (pagename file) (pagename file) content 
  where	isReadable = not.(any (=='\0'))
  	pre source = (tag "h1" file) ++
	             (tagP "a" [("href",file)] "(download)") ++
	  	     (tag "h2" "Source")++
		     (tag "pre" $ escape source)
  	binary   = (tag "h1" file) ++
	           (tagP "a" [("href",file)] "(download)") 

procCopyGen file wi = procCopy file wi >> procGeneric file wi

procCopy file wi = copyFile file (filename file)

