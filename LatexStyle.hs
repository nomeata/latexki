module LatexStyle (writeLatexPage, escapeL, envL, envLL) where

import System.Process
import System.IO


import Common
import Maybe

writeLatexPage wi file title basename body = do
  writeFileSafe (file ++ ".tex") $ latexFile wi title basename body
  readNull <- return.Just =<< openFile "/dev/null" ReadMode
  writeLog <- return.Just =<< openFile (file ++ ".output") WriteMode
  runProcess "pdflatex" [file++".tex"] Nothing Nothing readNull writeLog writeLog >>= waitForProcess


latexFile wi title basename body = 
  "\\documentclass{article}\n"++
  "\\usepackage[utf8]{inputenc}\n"++
  "\\usepackage{hyperref}\n"++
  "\\hypersetup{pdfpagemode=None,pdftitle="++title++",pdfpagelayout=OneColumn,pdfstartview=FitH,pdfview=FitH}"++
  "\\title{"++(escapeL title)++"}\n"++
  "\\begin{document}"++ body ++ "\\end{document}"
 {-
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
-}

escapes = [('\\',"\\textbackslash"),('&',"\\&"),('%',"\\%"),('#',"\\#"),('$',"\\$"),('^',"\\^"),('_',"\\_") ]
escapeL ""    = ""
escapeL (c:r) = (fromMaybe [c] $ lookup c escapes)  ++ escapeL r

envL name body  = "\\begin{"++name++"}"++body++"\\end{"++name++"}"
envLL name body  = ["\\begin{"++name++"}"]++body++["\\end{"++name++"}"]
