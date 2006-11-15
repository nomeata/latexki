module LatexStyle (writeLatexPage) where

import WikiData
import Common

import System.Process
import System.IO
import System.Directory
import System
import Maybe
import List


writeLatexPage wi file title basename body = do
  writeFileSafe (file ++ ".tex") $ latexFile wi title basename body
  cwd <- getCurrentDirectory
  readNull <- return.Just =<< openFile "/dev/null" ReadMode
  writeLog <- return.Just =<< openFile (file ++ ".output") WriteMode
  safeChdir (dirname file)
  err <- runProcess "pdflatex" [(filename file)++".tex"] Nothing Nothing readNull writeLog writeLog >>= waitForProcess
  setCurrentDirectory cwd
  case err of
	ExitFailure _ -> putStr (show err ++ ", PDF deleted") >> safeRemoveFile  (file ++ ".pdf")
	ExitSuccess   -> putStr "ok"
  


latexFile wi title basename body = 
  "\\documentclass{article}\n"++
  "\\usepackage[utf8]{inputenc}\n"++
  "\\usepackage[T1]{fontenc}\n"++
  "\\usepackage{hyperref}\n"++
  "\\usepackage{graphicx}\n"++
  "\\usepackage{textcomp}\n"++
  "\\DeclareUnicodeCharacter{2190}{\\textleftarrow}"++ -- really needed?
  "\\DeclareUnicodeCharacter{2192}{\\textrightarrow}"++ -- really needed?
  "\\hypersetup{pdfpagemode=None,pdftitle="++title++",pdfpagelayout=OneColumn,pdfstartview=FitH,pdfview=FitH}"++
  "\\title{"++(escape title)++"}\n"++
  "\\begin{document}"++ (concatMap render body) ++ "\\end{document}"
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

escapes = [	('\\',"\\textbackslash{}"),
		('&',"\\&{}"),
		('%',"\\%{}"),
		('#',"\\#{}"),
		('$',"\\${}"),
		('^',"\\textasciicircum{}"),
		('_',"\\_{}"),
		('~',"\\textasciitilde{}"),
		('{',"\\{{}"),
		('}',"\\}{}")
	]
escape ""    = ""
escape (c:r) = (fromMaybe [c] $ lookup c escapes)  ++ escape r

env name body  = "\\begin{"++name++"}"++body++"\\end{"++name++"}"

aHref href txt = "\\href{"++href++"}{"++txt++"}"

render (Paragraph text)  = "\n\n"++ (                       concatMap renderInline   text) ++"\n\n"
render (EnumList  items) = env "enumerate" (concatMap (("\\item "++).(concatMap renderInline)) items)
render (ItemList  items) = env "itemize"   (concatMap (("\\item "++).(concatMap renderInline)) items)
render (PreFormat str)   = env "verbatim"  (str)
render (HLine)           = "\n\\hrule\n"
render (Header 1 text) = "\\section*{"++       escape text ++"}"
render (Header 2 text) = "\\subsection*{"++    escape text ++"}"
render (Header 3 text) = "\\subsubsection*{"++ escape text ++"}"
render (Header 4 text) = "\\paragraph{"++      escape text ++"}"
render (Header _ text) = "\\textbf{"++         escape text ++"}"
render (RCElem [])     = ""
render (RCElem changes)  = env "enumerate" $ concatMap formatChange changes
  where	formatChange entry = ("\\item "++) $ env "description" $
                             concatMap (\(a,b) -> "\\item["++a ++"] "++ b ) [ 
  		("Revision:",                show         (revision entry)),
  		("Author:"  ,                escape       (author   entry)),
  		("Date:",                    escape       (date     entry)),
  		("Message:",       concatMap renderInline (message  entry)),
  		("Changed Files:",(env "itemize" $ concatMap (("\\item "++).renderLink) (links entry)))
		]

renderInline (Text str)      = escape str
renderInline (LinkElem link) = renderLink link
renderInline (Image src alt) = "\\includegraphics[width=\\linewidth]{"++ escape src ++"}"

renderLink (Link base txt (ext:exts)) = aHref (with ext) (escape txt) ++ more
  where with ext          = escape (base ++"."++ ext)
 	more | null exts  = ""
             | otherwise  = " ("++(concat $ intersperse ", " $ map (\e -> aHref (with e) (escape e)) exts)++")"

renderLink (NewLink base)       = aHref (escape (editLink base)) (escape (base ++ "(new)")) 
renderLink (DLLink file)        = aHref (escape file)            (escape (file ++ "(download)")) 
renderLink (PlainLink href txt) = aHref (escape href)            (escape txt)

linkto a = aHref (escape a) (escape a)

