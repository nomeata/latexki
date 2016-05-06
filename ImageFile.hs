module ImageFile ( procImage ) where


import Common
import WikiData
import HtmlStyle
import LatexStyle

import Generic

import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad


procImage :: FileProcessor
procImage image = liftM2 (++) (procCopy image) $ do 
        let htmlFile  = (pagename image) ++ ".html"
        let pdfFile  = (pagename image) ++ ".pdf"

        return [ ([htmlFile], writeHtmlPage htmlFile image (pagename image) content ) ]
  where content  = [Header 1 (B.pack (pagename image)),
                    Paragraph [Image link (B.pack (pagename image))],
                    Paragraph [LinkElem (DLLink link)]]
        link = B.pack (backDir image </> pageInput image)


