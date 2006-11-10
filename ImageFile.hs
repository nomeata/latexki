module ImageFile ( procImage ) where

import Common
import WikiData
import HtmlStyle
import LatexStyle

procImage image wi = do 
	let target  = (pagename image) ++ ".html"
	writeHtmlPage  wi target  (pagename image) (pagename image) content 
	writeLatexPage wi (pagename image)  (pagename image) (pagename image) content 
	writeFileSafe target $ htmlPage wi (pagename image) (pagename image) content 
  where	content  = [Header 1 (pagename image),
	            Paragraph [Image link (pagename image)],
	            Paragraph [LinkElem (DLLink link)]]
	link = backDir (pagename image) ++ image		 


