module Generic ( procGeneric ) where

import Common
import HtmlStyle
import Char

procGeneric file wi = do 
	source <- readFile file
	let content = if isReadable source then pre source else binary
	    target  = (basename file) ++ ".html"
	writeFile target $ htmlPage wi (basename file) content 
  where	isReadable = not.(any (=='\0'))
  	pre source = (tag "h1" file) ++
	             (tagP "a" [("href",file)] "(download)") ++
	  	     (tag "h2" "Source")++
		     (tag "pre" source)
  	binary   = (tag "h1" file) ++
	           (tagP "a" [("href",file)] "(download)") 
