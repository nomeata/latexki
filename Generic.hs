module Generic ( procGeneric, procCopyGen, procCopy ) where

import WikiData
import Common
import HtmlStyle

import Data.Char
import Data.Maybe
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad


procGeneric isBinary page = do 
	let htmlFile  = pageOutput page "html"
	-- needsUpdate <- anyOlder page [htmlFile]
	--liftIO $ showState (pagename page) depRes
	let source = pageSource page
	let content = if isNothing isBinary then 
		if isReadable source
				then pre source
				else binary
		else	if fromJust isBinary
				then binary
				else pre source
	return [ ([htmlFile], writeHtmlPage htmlFile page (pagename page) content) ]
  where	isReadable = ('\0' `B.notElem`)
  	pre source = [Header 1 (B.pack (pagename page)),
	              Paragraph [LinkElem (DLLink link)],
	  	      Header 2 (B.pack "Source"),
		      PreFormat source]
  	binary   = [Header 1 (B.pack (pagename page)),
	            Paragraph [LinkElem (DLLink link)]]
	link = B.pack (backDir page </> pageInput page)

procCopyGen page = liftM2 (++) (procCopy page) (procGeneric Nothing page)

procCopy :: FileProcessor
procCopy page = return [([outfile],  liftIO (copyFile (pageInput page) outfile))]
  where	outfile = pageOutput page (pageType page)

