module ImageFile ( procImage ) where

import Monad

import Common
import WikiData
import HtmlStyle
import LatexStyle
import Dependencies

procImage image wi = do 
	let htmlFile  = (pagename image) ++ ".html"
	let pdfFile  = (pagename image) ++ ".pdf"
	depRes <- liftIO $ needUpdate htmlFile [image]
	let up2date = isUpToDate depRes
	liftIO $ showState (pagename image) depRes
	unless up2date $ liftIO $ writeHtmlPage wi htmlFile (pagename image) (pagename image) content 
	producedFile htmlFile
	unless up2date $ liftIO $ writeLatexPage wi (pagename image) (pagename image) (pagename image) content 
	producedFile pdfFile
  where	content  = [Header 1 (pagename image),
	            Paragraph [Image link (pagename image)],
	            Paragraph [LinkElem (DLLink link)]]
	link = backDir (pagename image) ++ image	

