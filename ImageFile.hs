module ImageFile ( procImage ) where

import Common
import HtmlStyle

procImage image wi = do 
	let target  = (pagename image) ++ ".html"
	writeFileSafe target $ htmlPage wi (pagename image) (pagename image) content 
  where	content  = tag "h1" (pagename image) ++
	           tagP "img" [("src",link),("alt", pagename image)] "" ++ "<br/>" ++
	           tagP "a" [("href",link)] "(download)"
	link = backDir (pagename image) ++ image		 


