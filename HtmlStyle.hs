module HtmlStyle (htmlPage, tagP, tag, tagL, aHref, escape) where

import Common
import Maybe

htmlPage wi title basename body = 
  tag "html" ((
  	tag "head" ((
		tag "title" ((mainTitle wi)++" - "++title++"</title>")
  	)++(
		tagP "link" [("rel","stylesheet"),("type","text/css"),("href","latexki-style.css")] ""
	))
  )++(
  	tag "body" ((
		tagP "div" [("class","menu")] ( tag "ul" (
			concatMap li [	("Start page", "./"),
					("Edit this", "./cgi/edit/"++basename),
					("Create new page","./cgi/edit" )]	
		))
	)++(
		tagP "div" [("class","content")] body
	))
  ))
	where li (t,l) = tag "li" $ aHref l $ t

tagP name params body | null body = "<"++name++par++"/>"
                      | otherwise = "<"++name++par++">"++body++"</"++name++">"
	where par = concatMap (\(p,v)-> " "++p++"=\""++v++"\"") params 
tag name body= tagP name [] body
tagL name body= ["<"++name++">"]++body++["</"++name++">"]

aHref href body = tagP "a" [("href",href)] body

escapes = [('<',"&lt;"),('>',"&gt;"),('&',"&amp;"),('"',"&quot;") ]
escape ""    = ""
escape (c:r) = (fromMaybe [c] $ lookup c escapes)  ++ escape r
