{-# OPTIONS -fglasgow-exts #-}

module Wiki (procWiki) where


import FilePath
import List
import Char
import Data.Monoid
import Monad
import Maybe
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as B

import Common
import HtmlStyle
import LatexStyle
import WikiData
import Dependencies

alwaysUpdate text = mappend (if "!!sitemap!!" `subListOf` lc       then Always "Sitemap"       else UpToDate)
                            (if "!!recentchanges!!" `subListOf` lc then Always "RecentChanges" else UpToDate)
  where	lc = map toLower text

procWiki :: FileProcessor
procWiki wiki = do
	let htmlFile = pageOutput wiki "html"
	let pdfFile = pageOutput wiki "pdf"
	let content = B.unpack $ smContent wiki
	--depRes <- mappend (alwaysUpdate content) `fmap` needUpdate htmlFile [wiki]
	--let up2date = isUpToDate depRes
	--liftIO $ showState (pagename wiki) depRes
	wi <- getWi
	let parsed = parse wi $ map stripWhitespace $ lines content
	return [ 
		([htmlFile], writeHtmlPage htmlFile wiki (pagename wiki) parsed),
		([pdfFile],  writeLatexPage wiki  (pagename wiki) parsed)
		]

stripWhitespace = reverse.(dropWhile (==' ')).reverse

parse wi []                          = []
parse wi (l:r)	| null l             =                      parse wi r 
		| hl > 0             = Header hl header   : parse wi r
		| isSpecialLine l    = parseSpecial wi l  : parse wi r
		| isHLine l          = HLine              : parse wi r
		| isListLine l       = parseList wi (l:r)
		| isPreLine  l       = parsePre  wi (l:r)
		| isParaLine l       = parsePara wi (l:r)
		| otherwise          = error ("Unknown line "++l)
  where (hl, header) = parseHeader l	

parseHeader line | "=" `encloses` line  = add $ parseHeader $ takeout "=" line
                 | otherwise            = (0, line)
	where add (x,y) = (x+1,y)

isListLine = isPrefixOf "*"
isPreLine = isPrefixOf " "
isSpecialLine = ("!!" `encloses`)
isHLine l = length l >= 4 && all (`elem` "=-_") l
isParaLine l = not (isListLine l) && not (isPreLine l) && not (null l) &&
               not (fst (parseHeader l) > 0) && not (isSpecialLine l)

parseList wi = parseLines wi isListLine  ItemList  (parseInline wi . tail)
parsePre  wi = parseLines wi isPreLine  (PreFormat . unlines) tail
parsePara wi = parseLines wi isParaLine (Paragraph . concatMap (++[Text " "])) (parseInline wi)

parseLines :: forall t . WikiInfo 
              -> (String -> Bool)
              -> ([t] -> DocElement)
              -> (String -> t)
              -> [String]
              -> Document 
parseLines wi cond markup mapF lines	| null list = error "Did not find what I should parse"
					| otherwise = markup (map mapF list) : parse wi rest
	where (list,rest) = span cond lines

parseInline wi [] = []
parseInline wi t | isBlockedLink	 = Text skword                 : parseInline wi skrest
		 | isCamelCase word      = LinkElem (mkLink wi word)   : parseInline wi wrest 
                 | isBracketLink         = LinkElem (mkLink wi link)   : parseInline wi (tail lrest)
		 | isWebLink		 = LinkElem (PlainLink wlink wlink) :
		 							 parseInline wi wlrest
		 | not (null space)      = Text space                  : parseInline wi srest
		 | not (null word)       = Text word                   : parseInline wi wrest
		 | isBrokenLink          = Text [head t]               : parseInline wi (tail t)
		 | isBrokenBlock         = Text [head t]               : parseInline wi (tail t)
		 | otherwise             = error $ "Unhandled case in parseInline: "++t
  where	(link, lrest)     = span (not . (== ']')) (tail t)
  	(word, wrest)     = span isAlphaNum t
  	(skword, skrest)  = span isAlphaNum (tail t) -- skip !
  	(space, srest)    = span isNormalNonWord t
	(wlink, wlrest)   = span isWebLinkChar t
  	isNormalNonWord c = not (isAlphaNum c) && not (c `elem` "[!")
	isBlockedLink     = head t == '!' && isCamelCase skword
	isBrokenBlock	  = "!" `isPrefixOf` t && not isBlockedLink
	isBrokenLink	  = "[" `isPrefixOf` t && not isBracketLink
	isBracketLink     = "[" `isPrefixOf`t  && not (null lrest) && isValidPagename link 
	isWebLink         = "http://" `isPrefixOf` t
	isWebLinkChar c   = isAlphaNum c || c `elem` ":/_.-~?=" -- more to add?


isCamelCase []      = False
isCamelCase (w:ord) = isUpper w && any isUpper ord && any isLower ord && all isAlphaNum (w:ord) && all isAscii (w:ord)

mkPageLink wi page = WikiLink page (pagename page)

mkLink :: WikiInfo -> String -> Link
mkLink wi a = case lookupPage (PageName a) (sitemap wi) of 
	    	Just page -> WikiLink page a
	        Nothing   -> NewLink a

isValidPagename = all (\c -> isAlphaNum c || c `elem` "_-/" ) 

parseSpecial wi l = case map toLower $ takeout "!!" l of
			"hello"   -> Paragraph [Text "Hello World"]
			-- The next line is a beast
			"sitemap" -> ItemList $ map (\page -> [LinkElem (mkPageLink wi page)]) $ sort $ sitemap wi
			"recentchanges" -> RCElem (map (parseRC wi) (recentChanges wi))
			huh       -> Paragraph [Text ("Unknown Command \""++huh++"\"")]

parseRC wi (RawLogEntry rev auth date paths raw_msg) = LogEntry rev auth date links msg
  where msg = parseInline wi raw_msg
  	links = flip map paths $ \path -> case lookupPage (PageName (dropExtensions path)) (sitemap wi) of
			Just page -> mkPageLink wi page
			Nothing   -> NewLink (dropExtensions path)

encloses sub str = sub `isPrefixOf` str && sub `isSuffixOf` str && length str > 2 * length sub
takeout  sub    = (drop (length sub)).reverse.(drop (length sub)).reverse
(\/) pre post str = pre ++ str ++ post

