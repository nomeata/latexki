{-# LANGUAGE Rank2Types #-}
module Wiki (procWiki, depsWiki, alwaysWiki) where


import System.FilePath
import Data.List
import Data.Char
import Data.Monoid
import Data.Maybe
import Data.Functor
import Control.Applicative
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as B
import Safe

import BSUtils

import Common
import HtmlStyle
import LatexStyle
import WikiData

{-
import Dependency
alwaysUpdate text = mappend (if "!!sitemap!!" `subListOf` lc       then Always "Sitemap"       else UpToDate)
                            (if "!!recentchanges!!" `subListOf` lc then Always "RecentChanges" else UpToDate)
  where	lc = map toLower text
-}

alwaysWiki wi page = (B.pack "!!sitemap!!")       `subStringCI` (smContent page) ||
		     (B.pack "!!recentchanges!!") `subStringCI` (smContent page)

depsWiki :: WikiInfo -> PageInfo -> [PageInfo]
depsWiki wi wiki = mapMaybe (flip lookupPage (sitemap wi)) $ mapMaybe unLecture $ map parseSpecialLine $ filter isSpecialLine $ map stripWhitespace $ B.split '\n' $ smContent wiki
  where unLecture (Right (SpecialLecture name)) = Just (PageName (B.unpack name))
        unLecture _ = Nothing

procWiki :: FileProcessor
procWiki wiki = do
	let htmlFile = pageOutput wiki "html"
	let pdfFile = pageOutput wiki "pdf"
	let content = smContent wiki
	--depRes <- mappend (alwaysUpdate content) `fmap` needUpdate htmlFile [wiki]
	--let up2date = isUpToDate depRes
	--liftIO $ showState (pagename wiki) depRes
	wi <- getWi
	parsed <- parse wi $ map stripWhitespace $ B.split '\n' content
	return [ 
		([htmlFile], writeHtmlPage htmlFile wiki (pagename wiki) parsed),
		([pdfFile],  writeLatexPage wiki  (pagename wiki) parsed)
		]

stripWhitespace :: B.ByteString -> B.ByteString
stripWhitespace t | B.null t        = t
                  | B.last t == ' ' = stripWhitespace (B.init t)
		  | otherwise       = t

parse :: WikiInfo -> [B.ByteString] -> FileProducer Document
parse wi []                          = return []
parse wi (l:r)	| B.null l           =                      parse wi r 
		| hl > 0             = (Header hl header   :) <$> parse wi r
		| isSpecialLine l    = (:) <$> parseSpecial wi l <*> parse wi r
		| isHLine l          = (HLine              :) <$> parse wi r
		| isListLine l       = parseList wi (l:r)
		| isPreLine  l       = parsePre  wi (l:r)
		| isParaLine l       = parsePara wi (l:r)
		| otherwise          = error ("Unknown line "++ B.unpack l)
  where (hl, header) = parseHeader l	

parseHeader line | B.singleton '=' `encloses` line  = add $ parseHeader $ takeout (B.singleton '=') line
                 | otherwise            = (0, line)
	where add (x,y) = (x+1,y)

isListLine = B.isPrefixOf (B.singleton '*')
isPreLine = B.isPrefixOf (B.singleton ' ')
isSpecialLine = (B.pack "!!" `encloses`)
isHLine l = B.length l >= 4 && myAll (`elem` "=-_") l
isParaLine l = not (isListLine l) && not (isPreLine l) && not (B.null l) &&
               not (fst (parseHeader l) > 0) && not (isSpecialLine l)

parseList wi = parseLines wi isListLine  ItemList  (parseInline wi . B.tail)
parsePre  wi = parseLines wi isPreLine  (PreFormat . B.unlines) B.tail
parsePara wi = parseLines wi isParaLine (Paragraph . concatMap (++[Text (B.singleton ' ')])) (parseInline wi)

parseLines :: forall t . WikiInfo 
              -> (B.ByteString -> Bool)
              -> ([t] -> DocElement)
              -> (B.ByteString -> t)
              -> [B.ByteString]
              -> FileProducer Document 
parseLines wi cond markup mapF lines	| null list = error "Did not find what I should parse"
					| otherwise = (markup (map mapF list) :) <$> parse wi rest
	where (list,rest) = span cond lines

parseInline :: WikiInfo -> B.ByteString -> InlineText
parseInline wi t | B.null t              = []
                 | isBlockedLink	 = Text skword                 : parseInline wi skrest
		 | isCamelCase word      = LinkElem (mkLink wi word)   : parseInline wi wrest 
                 | isBracketLink         = LinkElem (mkLink wi link)   : parseInline wi (B.tail lrest)
		 | isWebLink		 = LinkElem (PlainLink wlink wlink) :
		 							 parseInline wi wlrest
		 | not (B.null space)    = Text space                  : parseInline wi srest
		 | not (B.null word)     = Text word                   : parseInline wi wrest
		 | isBrokenLink          = Text (B.take 1 t)           : parseInline wi (B.tail t)
		 | isBrokenBlock         = Text (B.take 1 t)           : parseInline wi (B.tail t)
		 | otherwise             = error $ "Unhandled case in parseInline: "++ B.unpack t
  where	(link, lrest)     = B.span (not . (== ']')) (B.tail t)
  	(word, wrest)     = B.span isAlphaNum t
  	(skword, skrest)  = B.span isAlphaNum (B.tail t) -- skip !
  	(space, srest)    = B.span isNormalNonWord t
	(wlink, wlrest)   = B.span isWebLinkChar t
  	isNormalNonWord c = not (isAlphaNum c) && not (c `elem` "[!")
	isBlockedLink     = B.head t == '!' && isCamelCase skword
	isBrokenBlock	  = B.singleton '!' `B.isPrefixOf` t && not isBlockedLink
	isBrokenLink	  = B.singleton '[' `B.isPrefixOf` t && not isBracketLink
	isBracketLink     = B.singleton '[' `B.isPrefixOf`t  && not (B.null lrest) && isValidPagename link 
	isWebLink         = (B.pack "http://") `B.isPrefixOf` t
	isWebLinkChar c   = isAlphaNum c || c `elem` ":/_.-~?=#" -- more to add?


isCamelCase t = not (B.null t) && isUpper w && myAny isUpper ord && myAny isLower ord && myAll isAlphaNum t && myAll isAscii t
  where (w,ord) = (B.head t, B.tail t)

mkPageLink wi page = WikiLink page (B.pack (pagename page))

mkLink :: WikiInfo -> B.ByteString -> Link
mkLink wi a = case lookupPage (PageName (B.unpack a)) (sitemap wi) of 
	    	Just page -> WikiLink page a
	        Nothing   -> NewLink (B.unpack a)

isValidPagename = myAll (\c -> isAlphaNum c || c `elem` "_-/" ) 

data Special = SpecialHello | SpecialSiteMap | SpecialRecentChanges 
    | SpecialLecture B.ByteString

parseSpecialLine :: B.ByteString -> Either B.ByteString Special
parseSpecialLine l | cmd == B.pack "hello"
                   = return SpecialHello
	           | cmd == B.pack "sitemap"
                   = return SpecialSiteMap
 	 	   | cmd == B.pack "recentchanges"
                   = return SpecialRecentChanges
                   | cmd == B.pack "lecture"
                   = case args of
                        [file] -> return (SpecialLecture file)
                        _ -> Left $ B.pack "Invalid argument to \"lecture\": " `B.append` B.unwords args
		   | otherwise
                   = Left $ B.pack "Unknown Command \"" `B.append` cmd `B.append` B.pack "\""
  where
    words = B.words $ takeout (B.pack "!!") l
    cmd = case words of
            [] -> B.pack ""
            c:_ -> B.map toLower c
    args = tail words

parseSpecial :: WikiInfo -> B.ByteString -> FileProducer DocElement
parseSpecial wi l = case parseSpecialLine l of
    Right SpecialHello ->
        return $ Paragraph [Text (B.pack "Hello World")]
    Right SpecialSiteMap ->
        return $ ItemList $ map (\page -> [LinkElem (mkPageLink wi page)]) $ sort $ sitemap wi
    Right SpecialRecentChanges ->
        return $ RCElem (map (parseRC wi) (recentChanges wi))
    Right (SpecialLecture file) ->
       genLiElem wi file
    Left err -> 
        return $ Paragraph [Text err]

genLiElem wi file = case lookupPage (PageName (B.unpack file)) (sitemap wi) of 
    Just page -> do
        let metaDataFile = pageOutput page "metadata"
        ex <- liftIO $ doesFileExist metaDataFile
        mbMd <- if ex then do
                metaDataText <- liftIO $ readFile metaDataFile
                (metaDataText == metaDataText) `seq` return ()
                case readMay metaDataText of
                    Just md -> return $ Just md
                    Nothing -> liftIO $ removeFile metaDataFile >> return Nothing
            else return Nothing
        return $ LIElem $ LectureInfo page mbMd
    Nothing   -> return $ Paragraph [Text (B.pack "Lecture file \"" `B.append` file `B.append` B.pack "\" not found.")]



parseRC wi (RawLogEntry rev auth date paths raw_msg) = LogEntry rev auth date links msg websvn
  where msg = parseInline wi raw_msg
  	links = flip map paths $ \path -> case lookupPage (PageName (dropExtensions path)) (sitemap wi) of
			Just page -> mkPageLink wi page
			Nothing   -> NewLink (dropExtensions path)
	websvn = fmap websvnlink (lookup "websvn" (wikiConfig wi))
	websvnlink url = PlainLink (B.pack (url ++ "?op=comp&compare[]=/@" ++ show (rev-1) ++ "&compare[]=/@" ++ show rev)) (B.pack "WebSVN Changes")

encloses sub str = sub `B.isPrefixOf` str && sub `myIsSuffixOf` str && B.length str > 2 * B.length sub
--encloses sub str = sub `isPrefixOf` str && sub `isSuffixOf` str && length str > 2 * length sub
takeout  sub str = B.drop (B.length sub) $ B.take (B.length str - B.length sub) $ str
--takeout  sub    = (drop (length sub)).reverse.(drop (length sub)).reverse
(\/) pre post str = pre ++ str ++ post

-- Missing ByteString functions
sub `myIsSuffixOf` str =  B.length str >= B.length sub && sub == (B.drop (B.length str - B.length sub) str)
myAll p s = B.foldr (\c b -> p c && b) True s
myAny p s = B.foldr (\c b -> p c || b) False s
