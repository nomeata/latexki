{-# OPTIONS_GHC -fimplicit-params #-}
module HtmlStyle (htmlPage, writeHtmlPage, tagP, tag, tagL, tagLP, aHref, escape) where


import Common
import WikiData
import System.FilePath
import Control.Monad

import Maybe
import List
import Char
import qualified Data.ByteString.Lazy.Char8 as B


writeHtmlPage file page title body = liftIO . (writeFileSafe file) =<< htmlPage page title body 

htmlPage :: PageInfo -> Maybe String -> String -> [DocElement] -> FileProducer (B.ByteString)
htmlPage page mbFlattr title body =  do
	mainTitle <- B.pack `liftM` getMainTitle
	wi <- getWi
	let ?currentPage = page
	let exts = pageExts page
	let addmenuconf   = fromMaybe "" . lookup "addmenu" $ wikiConfig wi
	    addmenu       =  map (\f -> (B.pack f,B.pack (bd (f++".html"))) ) $ words addmenuconf
	    viewMenu      =  map (\e -> (B.pack ("View as "++e),B.pack (bd (pageOutput page e))))  exts
	return $
	  B.pack "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" `B.append` (
	  tag (B.pack "html") ((
		tag (B.pack "head") ( B.concat [
			tagP (B.pack "meta") 
				[(B.pack "http-equiv",B.pack "Content-Type"),
				 (B.pack "content",B.pack "text/html; charset=UTF-8")] B.empty,
			tag (B.pack "title") $
				mainTitle `B.append` B.pack " - " `B.append` B.pack title,
			tagP (B.pack "link") 
				[(B.pack "rel",B.pack "stylesheet"),
				 (B.pack "type",B.pack "text/css"),
				 (B.pack "href",B.pack stylefile)     ] B.empty
		])
	  ) `B.append` (
		tag (B.pack "body") ((
			tagP (B.pack "div") [(B.pack "class", B.pack "menu")] $ B.concat 
			        [ ulist  $ (B.pack "Start page",B.pack (bd ".")) : addmenu 
				, ulist	 viewMenu
				, ulist	 [ (B.pack "Edit this", B.pack (bd (editLink page)))
				         , (B.pack "Create new page", B.pack (bd (newLink)))
					 ]
				] ++ ( case mbFlattr of
                                    Nothing -> []
                                    Just url -> [ulist [ (B.pack "Flattr this!", B.pack url ) ]]
                                )
			
		) `B.append` (
			tagP (B.pack "div") [((B.pack "class"),(B.pack "content"))] $
					    B.concat $ map render body
		))
	  )))

  where	li (t,l) = tag (B.pack "li") $ aHref l $ t
  	ulist    = tag (B.pack "ul") . B.concat . map li
      	stylefile = bd "latexki-style.css"
	bd = (backDir page </>)

tagP name params body | B.null body = B.singleton '<' `B.append` name `B.append` par `B.append` B.pack "/>"
                      | otherwise   = B.singleton '<' `B.append` name `B.append` par `B.append`
		      		      B.pack ">" `B.append` body `B.append` B.pack "</" `B.append` 
				      name `B.append` B.pack ">"
	where par = B.concat $ map (\(p,v)->
		B.singleton ' ' `B.append` p `B.append` B.pack "=\"" `B.append` v `B.append` B.pack "\""
		) params 
tag name body= tagP name [] body
tagL name body= tagLP name [] body
tagLP name params body= [B.singleton '<' `B.append` name `B.append` par `B.append` B.singleton '>']++
			body++
			[B.pack "</" `B.append` name `B.append` B.singleton '>']
	where par = B.concat $ map (\(p,v)->
		B.singleton ' ' `B.append` p `B.append` B.pack "=\"" `B.append` v `B.append` B.pack "\""
		) params 

aHrefRel href body = tagP (B.pack "a") [(B.pack "href", B.pack (backDir ?currentPage </> B.unpack href))] body
aHref href body = tagP (B.pack "a") [(B.pack "href",href)] body

escapes = [('<',B.pack "&lt;"),('>',B.pack "&gt;"),('&',B.pack "&amp;"),('"',B.pack "&quot;") ]
escape t | B.null t  = t
         | otherwise = case lookup (B.head t) escapes of
                        Just rep -> rep `B.append` escape (B.tail t)
                        Nothing  -> let (done,todo) = B.span (isNothing . flip lookup escapes) t in done `B.append` escape todo
{-
escape ""    = ""
escape (c:r) = (fromMaybe [c] $ lookup c escapes)  ++ escape r
-}

render (Paragraph text)  = tag (B.pack "p") $ B.concat $ 
				map renderInline text
render (EnumList  items) = tag (B.pack "ol") $ B.concat $ 
				map (tag (B.pack "li") . B.concat . map renderInline) items
render (ItemList  items) = tag (B.pack "ul") $ B.concat $
				map (tag (B.pack "li") . B.concat . map renderInline) items
render (PreFormat str)   = tag (B.pack "pre") (escape str)
render (HLine)           = tag (B.pack "hr") B.empty
render (Header lev text) = tag (B.pack ("h" ++ show lev)) (escape text)
render (RCElem changes)  = tagP (B.pack "ol") [(B.pack "id",B.pack "recentChanges")] $ B.concat $
				map formatChange changes
  where	formatChange entry = tag (B.pack "li") $ tag (B.pack "table") $ B.concat $
                             map (tag (B.pack "tr")) $
			     map (\(a,b) -> tag (B.pack "th") a `B.append` tag (B.pack "td") b ) $ [ 
  		(B.pack "Revision:", B.pack $   show         $ revision entry),
  		(B.pack "Author:",              escape       $ author   entry),
  		(B.pack "Date:",                escape       $ date     entry),
  		(B.pack "Message:", tag (B.pack "p") $  B.concat $
					map renderInline $ message entry),
  		(B.pack "Changed Files:", tag (B.pack "ul") $ B.concat $
					map (tag (B.pack "li") . renderLink) $ links  entry)
		] ++ (maybeToList $ fmap (\link -> (B.pack "Diff:", renderLink link)) (websvn entry))

renderInline (Text str)      = escape str
renderInline (LinkElem link) = renderLink link
renderInline (Image src alt) = tagP (B.pack "img") [(B.pack "src",escape src),(B.pack "alt",escape alt)] B.empty

renderLink (WikiLink page txt) = aHrefRel (escape (B.pack (pageOutput page "html"))) (escape txt) {- ++ more
  where with ext          = escape (base ++"."++ ext)
 	more | null exts  = ""
             | otherwise  = " ("++(concat $ intersperse ", " $ map (\e -> aHref (with e) (escape e)) exts)++")"
   -}

renderLink (NewLink page)       = aHrefRel (escape (B.pack (namedNewLink page)))
					   (escape (B.pack (page ++ " (new)")))
renderLink (DLLink file)        = aHrefRel (escape file)
					   (escape (file `B.append` B.pack" (download)")) 
renderLink (PlainLink href txt) = aHref (escape href)                (escape txt)


linkto a = aHref (escape a) (escape a)

