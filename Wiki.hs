module Wiki (procWiki) where

import FilePath
import List
import Char
import Common
import HtmlStyle

procWiki wiki wi = do
	content <- readFile wiki
	let formatted = (links wi) $ unlines $ lineBased $ lines content
	    target    = (basename wiki) ++ ".html"
	writeFile target $ htmlPage wi (basename wiki) formatted 

lineBased = prefo.paras.lists.(map headers).(map stripWhitespace)

stripWhitespace = reverse.(dropWhile (==' ')).reverse

headers line | hl == 0  = line
             | hl >  0  = "<h"++(show hl)++">"++header++"</h"++(show hl)++">"
	where (hl, header) = parseHeader line

parseHeader line | length line <= 3                     = (0, line)
                 | head line == '=' && last line == '=' = add $ parseHeader $ (tail.init) line
                 | otherwise                            = (0, line)
	where add (x,y) = (x+1,y)

groupLines cond markup lines | null list = cont
                             | otherwise = markup list ++ cont
	where (list,rest) = span cond lines
	      cont  | null rest = []
	            | otherwise = head rest:(groupLines cond markup (tail rest))

lists = groupLines (isPrefixOf "*") (\list -> tagL "ul" $ map ((tag "li").tail) list)
paras = groupLines isJustText (tagL "p")
prefo = groupLines (isPrefixOf " ") (tagL "pre")

isJustText l = not (isPrefixOf "<" l) &&
	       not (null l)

words' text = a : cont
	where (a,b) = span (isAlphaNum) text     
	      cont | null b    = []
	           | otherwise = [head b] : words' (tail b)

camelCase wi w | length w <= 3                            = w
               | any (not.isAlphaNum) w                   = w
               | isUpper (head w) && any isUpper (tail w) = linkPage wi w
            | otherwise                                = w

linkPage wi a | a `elem` basenames wi = (linkPageExt ext a) ++ more
              | otherwise             = a++"?"
 where linkPageExt ext txt = tagP "a" [("href", a ++"." ++ ext)] txt
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

