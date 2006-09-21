module Wiki (wiki2html) where

import FilePath
import List
import Char

import HtmlStyle

wiki2html wi wiki html = do
	let (_,basename,_) = splitFilePath wiki
	content <- readFile wiki
	let formatted = links $ unlines $ lineBased $ lines content
	writeFile html $ htmlPage wi basename formatted

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

camelCase w | length w <= 3                            = w
            | any (not.isAlphaNum) w                   = w
            | isUpper (head w) && any isUpper (tail w) = linkPage w
            | otherwise                                = w

linkPage a = tagP "a" [("href", a ++".html")] a

links = concat.links'.words'

links' [] = []
links' ("[":rest )  | null after                                  = "["           : links' rest
                    | isValidPagename link && (head after) == "]" = linkPage link : links' (tail after)
                    | otherwise                                   = "["           : links' rest
	where (linkParts,after) = span (/="]") rest
	      link = concat linkParts
links' (w1:rest) = camelCase w1 : links' rest

isValidPagename = all (\c -> isAlphaNum c || c `elem` "._-" ) 

bracketLinks = id

tagP name params body = "<"++name++concatMap (\(p,v)-> " "++p++"=\""++v++"\"") params ++">"++body++"</"++name++">"
tag name body= tagP name [] body
tagL name body= ["<"++name++">"]++body++["</"++name++">"]

