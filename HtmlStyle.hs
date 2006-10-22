module HtmlStyle (htmlPage, tagP, tag, tagL, tagLP, aHref, escape) where

import Common
import Maybe

htmlPage wi title basename body = 
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" ++ (
  tag "html" ((
  	tag "head" ( concat [
		tagP "meta" [("http-equiv","Content-Type"),("content","text/html; charset=UTF-8")] "",
		tag "title" ((mainTitle wi)++" - "++title++"</title>"),
		tagP "link" [("rel","stylesheet"),("type","text/css"),("href",stylefile)] ""
	])
  )++(
  	tag "body" ((
		tagP "div" [("class","menu")] ( tag "ul" (
			concatMap li ([	("Start page", "./") ] ++
					addmenu                ++
				      [ ("Edit this", "./cgi/edit/"++basename),
					("Create new page","./cgi/edit" )])
		))
	)++(
		tagP "div" [("class","content")] body
	))
  )))
	where li (t,l) = tag "li" $ aHref l $ t
	      addmenuconf = fromMaybe "" . lookup "addmenu" . wikiConfig  $ wi
	      addmenu =  map (\f -> (f,"./"++f++".html") ) $ words addmenuconf
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
