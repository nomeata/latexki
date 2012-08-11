{-# LANGUAGE ImplicitParams, RecordWildCards, OverloadedStrings #-}

module HtmlStyle (htmlPage, writeHtmlPage, tagP, tag, tagL, tagLP, aHref, escape) where


import Common
import WikiData
import System.FilePath
import Control.Monad

import Data.Maybe
import Data.List
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Time
import System.Locale

writeHtmlPage file page title body =
    liftIO . (writeFileSafe file) =<< htmlPage page title body 

htmlPage :: PageInfo -> String -> [DocElement] -> FileProducer (B.ByteString)
htmlPage page title body =  do
	mainTitle <- B.pack `liftM` getMainTitle
        mbFlattr <- getFlattrURL
	wi <- getWi
	let ?currentPage = page
	let exts = pageExts page
	let addmenuconf   = fromMaybe "" . lookup "addmenu" $ wikiConfig wi
	    addmenu       =  map (\f -> (B.pack f,B.pack (bd (f++".html"))) ) $ words addmenuconf
	    viewMenu      =  map (\e -> (B.pack ("View as "++e),B.pack (bd (pageOutput page e))))  exts
	return $
	  "<!doctype html>" `B.append` (
	  tag "html" ((
		tag "head" ( B.concat [
			tagP "meta" [("charset","utf-8")] B.empty,
			tag "title" $
				mainTitle `B.append` " - " `B.append` B.pack title,
			tagP "meta" 
				[("name","viewport"),
				 ("content","width=device-width, initial-scale=1.0")] B.empty,
                        tagP "meta"
                                [("http-equiv","X-UA-Compatible"),
                                 ("content","IE=edge,chrome=1")] B.empty,
			tagP "link" 
				[("rel","stylesheet"),
				 ("media","screen"),
				 ("href",B.pack stylefile)     ] B.empty
		])
	  ) `B.append` (
		tag "body" $ B.concat [
			tagP "header" [("id", "msw_top")] $ B.concat $
			        [ tag "h1" $ aHref "." mainTitle
                                , tag "nav" $ ulist $ [
                                        (UTF8.fromString "⌂  Home",B.pack (bd "."))
                                      , (UTF8.fromString "✑  Guestbook",B.pack (bd "IchWarHier.html"))
                                      --, (UTF8.fromString "⚒  HowTo",B.pack (bd "HowTo.html"))
                                      , (UTF8.fromString "✱  Sitemap",B.pack (bd "SiteMap.html"))
                                      , (UTF8.fromString "⌚  Wiki-History",B.pack (bd "RecentChanges.html"))
                                      , (UTF8.fromString "⌘  About",B.pack (bd "About.html"))
                                      ]
                                , hr
                                ]
		, tagP "div" [("class","body")] $
		            B.concat $ [
                                tagP "nav" [("class", "page_nav")] $ ulist' $ [
                                        ("editPage","Edit this page",B.pack (bd (editLink page)),UTF8.fromString "✎")
                                      --, ("history","Changes",".",UTF8.fromString "↺")
                                      , ("new","New Document",B.pack (bd newLink),UTF8.fromString "✚")
                                      , ("totop","Back to top","#msw_top",UTF8.fromString "⬆")
                                      ]
                            , tagP "div" [("class","page")] $ B.concat $ [ hr ] ++ map render body
                            ]
		, tag "footer" $ B.concat [
                      hr
                    , tag "ul" $ B.concat [
                        tag "li" $ tagP "div" [("id","socialshareprivacy")] B.empty
                        ]
                    , tagP "div" [("class", "hr hr2")] $ tag "hr" B.empty
                    ]
                , B.concat $ map (\s -> tagP "script" [("src", B.pack $ bd s)] " ") [
                      "js/minimized.js"
                    ]
                ]
	  )))

  where	li (t,l) = tag "li" $ aHref l $ t
        li' (c,t,l,d) = tagP "li" [("class",c)] $ tagP "a" [("href",l),("title",t)] d
  	ulist    = tag "ul" . B.concat . map li
  	ulist'    = tag "ul" . B.concat . map li'
      	stylefile = bd "latexki-style.css"
	bd = (backDir page </>)
        hr = tagP "div" [("class", "hr")] $ tag "hr" B.empty


tagP name params body | B.null body = B.singleton '<' `B.append` name `B.append` par `B.append` "/>"
                      | otherwise   = B.singleton '<' `B.append` name `B.append` par `B.append`
		      		      ">" `B.append` body `B.append` "</" `B.append` 
				      name `B.append` ">"
	where par = B.concat $ map (\(p,v)->
		B.singleton ' ' `B.append` p `B.append` "=\"" `B.append` v `B.append` "\""
		) params 
tag name body= tagP name [] body
tagL name body= tagLP name [] body
tagLP name params body= [B.singleton '<' `B.append` name `B.append` par `B.append` B.singleton '>']++
			body++
			["</" `B.append` name `B.append` B.singleton '>']
	where par = B.concat $ map (\(p,v)->
		B.singleton ' ' `B.append` p `B.append` "=\"" `B.append` v `B.append` "\""
		) params 

aHrefRelClassTitle title c href body = tagP "a" [("class",c), ("href", B.pack (backDir ?currentPage </> B.unpack href)),("title",title)] body
aHrefRelClass c href body = tagP "a" [("class",c), ("href", B.pack (backDir ?currentPage </> B.unpack href))] body
aHrefRel href body = tagP "a" [("href", B.pack (backDir ?currentPage </> B.unpack href))] body
aHref href body = tagP "a" [("href",href)] body

escapes = [('<',"&lt;"),('>',"&gt;"),('&',"&amp;"),('"',"&quot;") ]
escape t | B.null t  = t
         | otherwise = case lookup (B.head t) escapes of
                        Just rep -> rep `B.append` escape (B.tail t)
                        Nothing  -> let (done,todo) = B.span (isNothing . flip lookup escapes) t in done `B.append` escape todo
{-
escape ""    = ""
escape (c:r) = (fromMaybe [c] $ lookup c escapes)  ++ escape r
-}

render (Paragraph text)  = tag "p" $ B.concat $ 
				map renderInline text
render (EnumList  items) = tag "ol" $ B.concat $ 
				map (tag "li" . B.concat . map renderInline) items
render (ItemList  items) = tag "ul" $ B.concat $
				map (tag "li" . B.concat . map renderInline) items
render (PreFormat str)   = tag "pre" (escape str)
render (HLine)           = tag "hr" B.empty
render (Header lev text) = tag (B.pack ("h" ++ show lev)) (escape text)
render (LIElem li)       = renderLi li
render (RCElem changes)  = tagP "ol" [("id","recentChanges")] $ B.concat $ map formatChange changes
  where	formatChange entry = tag "li" $ tag "table" $ B.concat $
                             map (tag "tr") $
			     map (\(a,b) -> tag "th" a `B.append` tag "td" b ) $ [ 
  		("Revision:", B.pack $   show         $ revision entry),
  		("Author:",              escape       $ author   entry),
  		("Date:",                escape       $ B.pack $ formatTime defaultTimeLocale rfc822DateFormat $ date entry),
  		("Message:", tag "p" $  B.concat $
					map renderInline $ message entry)
                ] ++ (
                    if null (links entry) then [] else
                    [("Changed Files:", tag "ul" $ B.concat $ map (tag "li" . renderLink) $ links  entry)]
                ) ++ (
                    maybeToList $ fmap (\link -> ("Diff:", renderLink link)) (websvn entry)
                )

renderInline (Text str)      = escape str
renderInline (LinkElem link) = renderLink link
renderInline (Image src alt) = tagP "img" [("src",escape src),("alt",escape alt)] B.empty

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

renderLi (LectureInfo page Nothing) = tagP "div" [("class", "lecture")] $ B.concat [
    "No meta data known about "
    , aHrefRel (escape (B.pack (pageOutput page "html"))) (escape (B.pack (show (smPageName page))))
    ]
renderLi (LectureInfo page (Just (MetaData {..}))) = tagP "div" [("class", "lecture")] $ B.concat $ [
    tag "h3" $ escape mdTitle `B.append` aHrefRelClass "edit" (escape (B.pack (editLink page))) (UTF8.fromString "✎ Edit")
    , case mdPDFData of 
        Nothing -> classedSpan "pdf broken" $ 
                tag "span" "PDF" `B.append` tagP "img" [("src",B.pack (backDir ?currentPage </> "res/cover-broken.png")),("title","No PDF file could be created for this document.")] B.empty
        Just (PDFData {..}) ->
            aHrefRelClass "pdf" (B.pack (backDir ?currentPage </> pageOutput page "pdf")) $
                tag "span" "PDF" `B.append` tagP "img" [("src",B.pack (backDir ?currentPage </> pageOutput page "png")), ("title", "PDF-File (" `B.append` B.pack (show numberOfPages) `B.append` " pages)")] B.empty
    , tag "p" $ B.concat $ intersperse " | " $ catMaybes [
          fmap (classedSpan "lecturer" . escape) mdLecturer
        , fmap (classedSpan "semester" . escape) mdSemester
        ]
    , tag "h4" "More"
    , tagP "div" [("class","more")] $ B.concat $ [
          tagP "p" [("class","changed")] $ B.concat [
              "Last change ",
              escape $ B.pack $ formatTime defaultTimeLocale rfc822DateFormat $ dateR mdLastChange,
              " by ",
              escape (authorR mdLastChange)
            ]
        , tag "div" $
            (if length mdIndex > 1
            then B.concat [
                tagP "a" [("class","openDialog"), ("href","#")]
                    "Inhaltsverzeichnis / kapitelweises bearbeiten"
                , tagP "div" [("class","dialogMessage"), ("title","Inhaltsverzeichnis")] $
                    tag "ol" $ B.concat $
                        let toContentLi (TexIndex {..}) = tag "li" $ B.concat [
                                tiTitle , " ",
                                -- aHrefRelClass "pdf" "#" "PDF",
                                aHrefRelClass "edit" (escape (B.pack (editLinkLines page tiPageFrom tiPageTo))) (UTF8.fromString "✎ Edit")
                                ]
                        in map toContentLi mdIndex
                ]
            else B.empty)
            `B.append` B.concat [
              tag "p" $ classedSpan "latexhtml" $ B.concat [
                  classedSpan "l1" "L"
                , classedSpan "l2" "a"
                , classedSpan "l3" "T"
                , classedSpan "l4" "e"
                , classedSpan "l5" "X"
                , ": "
                , aHrefRelClass "info" (escape (B.pack (pageInput page))) (escape "Source")
                , " | "
                , aHrefRelClass "info" (escape (B.pack (pageOutput page "output"))) (escape "Output")
                , " | "
                , aHrefRelClass "info" (escape (B.pack (pageOutput page "log"))) (escape "Logfile")
                ]
            ]
        ]
    ]

linkto a = aHref (escape a) (escape a)

classedSpan c a = tagP "span" [("class", B.pack c)] a
br = tag "br" ""
