module HtmlStyle (htmlPage, writeHtmlPage, tagP, tag, tagL, tagLP, aHref, escape) where

import Common
import WikiData

import Maybe
import List

writeHtmlPage file title basename body = liftIO . (writeFileSafe file) =<< htmlPage title basename body 

htmlPage title basename body = do
	mainTitle <- getMainTitle
	wikiConfig <- getWikiConfig
	sitemap <- getSiteMap
	let exts basename = triple3 $ head $ filter ((==basename).triple1) sitemap
	let   addmenuconf = fromMaybe "" . lookup "addmenu" $ wikiConfig
	      addmenu =  (map (\f -> (f,"./"++f++".html") ) $ words addmenuconf) ++
	                 (map (\e -> ("View as "++e,"./"++basename++"."++e)) $ exts basename)
	return $
	  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" ++ (
	  tag "html" ((
		tag "head" ( concat [
			tagP "meta" [("http-equiv","Content-Type"),("content","text/html; charset=UTF-8")] "",
			tag "title" (mainTitle++" - "++title++"</title>"),
			tagP "link" [("rel","stylesheet"),("type","text/css"),("href",stylefile)] ""
		])
	  )++(
		tag "body" ((
			tagP "div" [("class","menu")] ( tag "ul" (
				concatMap li ([	("Start page", "./") ] ++
						addmenu                ++
					      [ ("Edit this", editLink basename),
						("Create new page",editLink "" )])
			))
		)++(
			tagP "div" [("class","content")] (concatMap render body)
		))
	  )))
	where li (t,l) = tag "li" $ aHref l $ t
	      stylefile = backDir basename ++ "latexki-style.css"

tagP name params body | null body = "<"++name++par++"/>"
                      | otherwise = "<"++name++par++">"++body++"</"++name++">"
	where par = concatMap (\(p,v)-> " "++p++"=\""++v++"\"") params 
tag name body= tagP name [] body
tagL name body= tagLP name [] body
tagLP name params body= ["<"++name++par++">"]++body++["</"++name++">"]
	where par = concatMap (\(p,v)-> " "++p++"=\""++v++"\"") params 

aHref href body = tagP "a" [("href",href)] body

escapes = [('<',"&lt;"),('>',"&gt;"),('&',"&amp;"),('"',"&quot;") ]
escape ""    = ""
escape (c:r) = (fromMaybe [c] $ lookup c escapes)  ++ escape r

render (Paragraph text)  = tag "p"  (                       concatMap renderInline   text)
render (EnumList  items) = tag "ol" (concatMap (tag "li" . (concatMap renderInline)) items)
render (ItemList  items) = tag "ul" (concatMap (tag "li" . (concatMap renderInline)) items)
render (PreFormat str)   = tag "pre" (escape str)
render (HLine)           = tag "hr" ""
render (Header lev text) = tag ("h" ++ (show lev)) (escape text)
render (RCElem changes)  = tagP "ol" [("id","recentChanges")] $ concatMap formatChange changes
  where	formatChange entry = tag "li" $ tag "table" $
                             concatMap (tag "tr") $ map (\(a,b) -> tag "th" a ++ tag "td" b ) [ 
  		("Revision:",                show         (revision entry)),
  		("Author:",                  escape       (author   entry)),
  		("Date:",                    escape       (date     entry)),
  		("Message:",      (tag "p"  $ concatMap (renderInline)          (message entry))),
  		("Changed Files:",(tag "ul" $ concatMap (tag "li" . renderLink) (links   entry)))
		]

renderInline (Text str)      = escape str
renderInline (LinkElem link) = renderLink link
renderInline (Image src alt) = tagP "img" [("src",escape src),("alt",escape alt)] ""

renderLink (WikiLink base txt) = aHref (escape (base ++".html")) (escape txt) {- ++ more
  where with ext          = escape (base ++"."++ ext)
 	more | null exts  = ""
             | otherwise  = " ("++(concat $ intersperse ", " $ map (\e -> aHref (with e) (escape e)) exts)++")"
   -}

renderLink (NewLink base)       = aHref (escape (editLink base)) (escape (base ++ "(new)")) 
renderLink (DLLink file)        = aHref (escape file)            (escape (file ++ "(download)")) 
renderLink (PlainLink href txt) = aHref (escape href)            (escape txt)


linkto a = aHref (escape a) (escape a)

