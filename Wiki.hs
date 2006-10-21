module Wiki (wikiDeps, procWiki) where

import FilePath
import List
import Char
import Maybe

import Common
import HtmlStyle

wikiDeps wiki wi = do
	content <- readFile wiki
	let lc = map toLower content
	let sitemap = if "##sitemap##"       `subListOf` lc then Just FileList          else Nothing
	let repch   = if "##recentchanges##" `subListOf` lc then Just RepositoryChanges else Nothing
	return $ FileDep wiki : catMaybes [sitemap,repch]

procWiki wiki wi = do
	content <- readFile wiki
	let formatted = links wi $ unlines $ lineBased wi $ lines $ escape content
	    target    = (basename wiki) ++ ".html"
	writeFile target $ htmlPage wi (basename wiki) (basename wiki) formatted 
	debug wi "ok"

lineBased wi = prefo.paras.lists.(map headers).(specials wi).(map stripWhitespace)

stripWhitespace = reverse.(dropWhile (==' ')).reverse

headers line | hl == 0  = line
             | hl >  0  = tag ("h"++(show hl)) header
	where (hl, header) = parseHeader line

parseHeader line | "=" `encloses` line  = add $ parseHeader $ takeout "=" line
                 | otherwise            = (0, line)
	where add (x,y) = (x+1,y)

groupLines cond markup lines | null list = cont
                             | otherwise = markup list ++ cont
	where (list,rest) = span cond lines
	      cont  | null rest = []
	            | otherwise = head rest:(groupLines cond markup (tail rest))

lists = groupLines (isPrefixOf "*") ((tagL "ul").(map ((tag "li").tail)))
paras = groupLines isJustText       ( tagL "p")
prefo = groupLines (isPrefixOf " ") ((tagL "pre").(map tail))

isJustText l = not (isPrefixOf "<" l) &&
	       not (null l)

words' text = a : cont
	where (a,b) = span (isAlphaNum) text     
	      cont | null b    = []
	           | otherwise = [head b] : words' (tail b)

camelCase wi w = if isCamelCase w then linkPage wi w else w
isCamelCase []      = False
isCamelCase (w:ord) = isUpper w && any isUpper ord && any isLower ord && all isAlphaNum (w:ord)

linkPage wi a | a `elem` basenames wi = (linkPageExt ext a) ++ more
              | otherwise             = a
 where linkPageExt ext txt = aHref(a ++"." ++ ext) txt
       (ext:exts) = triple3 $ head $ filter ((==a).triple1) (sitemap wi)
       more | null exts  = ""
            | otherwise  = " ("++(concat $ intersperse ", " $ map (\e -> linkPageExt e e) exts)++")"

links wi = concat.(links').words'
  where links' [] = []
	links' ("[":rest )  | null after                                  = "["              : links' rest
	                    | isValidPagename link && (head after) == "]" = linkPage wi link : links' (tail after)
	                    | otherwise                                   = "["              : links' rest
	  where (linkParts,after) = span (/="]") rest
	        link = concat linkParts
	links' (w1:rest) = camelCase wi w1 : links' rest

isValidPagename = all (\c -> isAlphaNum c || c `elem` "._-" ) 

specials wi []       = []
specials wi  (line:r) | "##" `encloses` line = (case map toLower $ takeout "##" line of
				  		"hello"   -> ["Hello World"]
						-- The next line is cool.
						"sitemap" -> map ("* ["\/"]") $ sort $ basenames wi
						"recentchanges" -> formatRC (recentChanges wi)
						huh       -> ["Unknown Command \""++huh++"\""] 
				           ) ++ specials wi r
	 	      | otherwise            = line : specials wi r

formatRC = tagL "ol" . map formatChange
  where	formatChange entry = tag "li" $ 
  		"Revision : "++(show (revision entry)) ++"<br/>" ++
  		"Author   : "++(      author   entry ) ++"<br/>" ++
  		"Date     : "++(      date   entry ) ++"<br/>"++
  		"Message  : "++(tag "p" $ message entry ) ++
  		"Changed Files: "++(tag "ul" $ concatMap (
				tag "li" . ("["\/"]"). basename
			) (paths entry) ) 


encloses sub str = sub `isPrefixOf` str && sub `isSuffixOf` str && length str > 2 * length sub
takeout  sub    = (drop (length sub)).reverse.(drop (length sub)).reverse
(\/) pre post str = pre ++ str ++ post
