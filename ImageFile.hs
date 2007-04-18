module ImageFile ( procImage ) where

import Monad

import Common
import WikiData
import HtmlStyle
import LatexStyle
import Dependencies

import System.FilePath

procImage :: FileProcessor
procImage image = do 
	let htmlFile  = (pagename image) ++ ".html"
	let pdfFile  = (pagename image) ++ ".pdf"
	return [
		([htmlFile], writeHtmlPage htmlFile image (pagename image) content ),
		([pdfFile], writeLatexPage image (pagename image) content)
		]
  where	content  = [Header 1 (pagename image),
	            Paragraph [Image link (pagename image)],
	            Paragraph [LinkElem (DLLink link)]]
	link = backDir image </> pageInput image


