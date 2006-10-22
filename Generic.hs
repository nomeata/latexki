module Generic ( procGeneric, procCopyGen, procCopy ) where

import Common
import HtmlStyle
import Char
import Maybe
import Monad
import System.Directory

procGeneric isBinary file wi = do 
	content <- 
		if isNothing isBinary then do
			source <- readFile file
			return $ if isReadable source then pre source else binary
		  else	if fromJust isBinary then return binary
			else pre `liftM` readFile file
	writeFileSafe target $ htmlPage wi (pagename file) (pagename file) content 
  where	isReadable = not.(any (=='\0'))
	target  = (pagename file) ++ ".html"
  	pre source = (tag "h1" file) ++
	             (tagP "a" [("href",link)] "(download)") ++
	  	     (tag "h2" "Source")++
		     (tag "pre" $ escape source)
  	binary   = (tag "h1" file) ++
	           (tagP "a" [("href",link)] "(download)") 
	link = backDir (pagename file) ++ file

procCopyGen file wi = procCopy file wi >> procGeneric Nothing file wi

procCopy file wi = copyFile file (filename file)

