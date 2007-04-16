module Generic ( procGeneric, procCopyGen, procCopy ) where

import WikiData
import Common
import HtmlStyle
import Dependencies

import Char
import Maybe
import Monad
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as B


procGeneric isBinary page = do 
	let htmlFile  = pageOutput page "html"
	needsUpdate <- anyOlder page [htmlFile]
	--liftIO $ showState (pagename page) depRes
	when needsUpdate $ do
		let source = pageSource page
		let content = if isNothing isBinary then 
			if isReadable source
					then pre source
					else binary
			else	if fromJust isBinary
					then binary
					else pre source
		writeHtmlPage htmlFile page (pagename page) content 
	producedFile htmlFile
  where	isReadable = ('\0' `B.notElem`)
  	pre source = [Header 1 (pagename page),
	              Paragraph [LinkElem (DLLink link)],
	  	      Header 2 "Source",
		      PreFormatBS source]
  	binary   = [Header 1 (pagename page),
	            Paragraph [LinkElem (DLLink link)]]
	link = backDir page </> pageInput page

procCopyGen page = procCopy page >> procGeneric Nothing page 

procCopy page = liftIO (copyFile (pageInput page) outfile) >> producedFile outfile
  where	outfile = pageOutput page (pageType page)

