module ImageFile ( procImage ) where

import Monad

import Common
import WikiData
import HtmlStyle
import LatexStyle
import Dependencies

import System.FilePath

procImage image = do 
	let htmlFile  = (pagename image) ++ ".html"
	let pdfFile  = (pagename image) ++ ".pdf"
	needsUpdate <- anyOlder image [htmlFile]
	-- liftIO $ showState (pagename image) depRes
	when needsUpdate $ writeHtmlPage htmlFile image (pagename image) content 
	producedFile htmlFile
	when needsUpdate $ writeLatexPage image (pagename image) content 
	producedFile pdfFile
  where	content  = [Header 1 (pagename image),
	            Paragraph [Image link (pagename image)],
	            Paragraph [LinkElem (DLLink link)]]
	link = backDir image </> pageInput image


