module Generic ( generic2html ) where

import Common
import HtmlStyle
import Char

generic2html wi file html = do 
	source <- readFile file
	let content = if isReadable source then pre source else binary
	writeFile html $ htmlPage wi (basename file) content 
  where	isReadable = not.(any (=='\0'))
  	pre source = (tag "h1" file) ++
	             (tagP "a" [("href",file)] "(download)") ++
	  	     (tag "h2" "Source")++
		     (tag "pre" source)
  	binary   = (tag "h1" file) ++
	           (tagP "a" [("href",file)] "(download)") 
