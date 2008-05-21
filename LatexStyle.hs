module LatexStyle (writeLatexPage) where

import WikiData
import Common

import System.Process
import System.IO
import System.Directory
import System
import System.FilePath
import Maybe
import Control.Monad
import List
import qualified Data.ByteString.Lazy.Char8 as B


writeLatexPage page title body = do
  liftIO  . (writeFileSafe (pageOutput page ".tex")) =<< latexFile page title body
  liftIO $ do
	  readNull <- Just `liftM` openFile "/dev/null" ReadMode
	  writeLog <- Just `liftM` openFileSafe (pageOutput page ".output") WriteMode
	  err <- inTargetDir page $ do
	  	putStrLn $ "Running pdflatex "++takeBaseName (fileRelative page) <.> "tex"
	  	runProcess "pdflatex" [takeBaseName (fileRelative page) <.> "tex"] Nothing Nothing readNull writeLog writeLog >>= waitForProcess
	  case err of
		ExitFailure _ -> putStrLn (pagename page ++ ": LaTeX failed ("++show err ++")")
		ExitSuccess   -> return ()
  


latexFile page title  body = return $ B.concat [
  B.pack "\\documentclass{article}\n",
  B.pack "\\usepackage[utf8]{inputenc}\n",
  B.pack "\\usepackage[T1]{fontenc}\n",
  B.pack "\\usepackage{hyperref}\n",
  B.pack "\\usepackage{graphicx}\n",
  B.pack "\\usepackage{textcomp}\n",
--  B.pack "\\DeclareUnicodeCharacter{2190}{\\textleftarrow}", -- really needed?
--  B.pack "\\DeclareUnicodeCharacter{2192}{\\textrightarrow}"++ -- really needed?
  B.pack "\\hypersetup{pdfpagemode=None,pdftitle=",B.pack title,
		  	B.pack ",pdfpagelayout=OneColumn,pdfstartview=FitH,pdfview=FitH}",
  B.pack "\\title{", escape (B.pack title), B.pack "}\n",
  B.pack "\\begin{document}",
  B.concat (map render body),
  B.pack "\\end{document}"
  ]
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

file_escapes = [
		('\\',B.pack "\\textbackslash{}"),
		('{',B.pack "\\{{}"),
		('}',B.pack "\\}{}")
	]
escapes = file_escapes ++ [	
		('&',B.pack "\\&{}"),
		('%',B.pack "\\%{}"),
		('#',B.pack "\\#{}"),
		('$',B.pack "\\${}"),
		('^',B.pack "\\textasciicircum{}"),
		('_',B.pack "\\_{}"),
		('~',B.pack "\\textasciitilde{}")
	]
escape' l t | B.null t  = t
            | otherwise = case lookup (B.head t) l of 
	 		Just rep -> rep `B.append` escape (B.tail t)
			Nothing  -> let (done,todo) = B.span (isNothing . flip lookup l) t in done `B.append` escape todo

escape = escape' escapes
file_escape = escape' file_escapes

{-
escape ""    = ""
escape (c:r) = (fromMaybe [c] $ lookup c escapes)  ++ escape r
-}


env name body = B.concat [B.pack "\\begin{",name,B.singleton '}',body,B.pack "\\end{",name,B.singleton '}']

aHref href txt = B.concat [B.pack "\\href{", href, B.pack "}{", txt, B.pack "}"]

renderInlineText  = B.concat . map renderInline
render (Paragraph text)  = B.pack "\n\n" `B.append` renderInlineText text `B.append` B.pack "\n\n"
render (EnumList  items) = env (B.pack "enumerate")
                               (B.concat (map ((B.pack "\\item " `B.append`). renderInlineText) items))
render (ItemList  items) = env (B.pack "itemize")
                               (B.concat (map ((B.pack "\\item " `B.append`). renderInlineText) items))
render (PreFormat str)   = env (B.pack "verbatim")  (escape str)
render (HLine)           = B.pack "\n\\hrule\n"
render (Header 1 text) = B.pack "\\section*{"       `B.append` escape text `B.append` B.pack "}"
render (Header 2 text) = B.pack "\\subsection*{"    `B.append` escape text `B.append` B.pack "}"
render (Header 3 text) = B.pack "\\subsubsection*{" `B.append` escape text `B.append` B.pack "}"
render (Header 4 text) = B.pack "\\paragraph{"      `B.append` escape text `B.append` B.pack "}"
render (Header _ text) = B.pack "\\textbf{"         `B.append` escape text `B.append` B.pack "}"
render (RCElem [])     = B.empty
render (RCElem changes)  = env (B.pack "enumerate") $ B.concat $ map formatChange changes
  where	formatChange entry = (B.pack "\\item " `B.append`) $ env (B.pack "description") $
                             B.concat $ map (\(a,b) -> B.concat [B.pack "\\item[",a,B.pack "] ", b] ) $ [ 
  		(B.pack "Revision:",B.pack $             show   $ revision entry),
  		(B.pack "Author:"  ,                     escape $ author   entry),
  		(B.pack "Date:",                         escape $ date     entry),
  		(B.pack "Message:", B.concat $ map renderInline $ message  entry),
  		(B.pack "Changed Files:",(env (B.pack "itemize") $ B.concat $
					map ((B.pack "\\item " `B.append`) . renderLink) (links entry)))
		] ++ (maybeToList $ fmap (\link -> (B.pack "Diff:", renderLink link)) (websvn entry))

renderInline (Text str)      = escape str
renderInline (LinkElem link) = renderLink link
renderInline (Image src alt) = B.pack "\\includegraphics[width=\\linewidth]{" `B.append` file_escape src `B.append` B.pack "}"

renderLink (WikiLink page txt) = aHref (escape (B.pack (pageOutput page "html"))) (escape txt) {-++ more
  where with ext          = escape (base ++"."++ ext)
 	more | null exts  = ""
             | otherwise  = " ("++(concat $ intersperse ", " $ map (\e -> aHref (with e) (escape e)) exts)++")"
-}

renderLink (NewLink page)       = aHref (escape (B.pack (namedNewLink page))) (escape (B.pack (page ++ " (new)")))
renderLink (DLLink file)        = aHref (escape file)                         (escape (file `B.append` B.pack " (download)"))
renderLink (PlainLink href txt) = aHref (escape href)                         (escape txt)

linkto a = aHref (escape a) (escape a)

