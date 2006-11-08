module Wiki (wikiDeps, procWiki) where

import FilePath
import List
import Char
import Maybe

import Common
import HtmlStyle
import LatexStyle

wikiDeps wiki wi = do
	content <- readFile wiki
	let lc = map toLower content
	let sitemap = if "!!sitemap!!"       `subListOf` lc then Just FileList          else Nothing
	let repch   = if "!!recentchanges!!" `subListOf` lc then Just RepositoryChanges else Nothing
	return $ FileDep wiki : catMaybes [sitemap,repch]

procWiki wiki wi = do
	content <- readFile wiki
	let formatted  = links  wi $ unlines $ lineBased  wi $ lines $ escape  content
	    formattedL = linksL wi $ unlines $ lineBasedL wi $ lines $ escapeL content
	writeHtmlPage wi (pagename wiki ++ ".html") (pagename wiki) (pagename wiki) formatted 
	writeLatexPage wi (pagename wiki) (pagename wiki) (pagename wiki)  formattedL
	debug wi "ok"

lineBasedL wi = prefoL.parasL.listsL.(map headersL).(specials formatRCL wi).(map stripWhitespace)
lineBased  wi = prefo. paras. lists. (map headers) .(specials formatRC wi) .(map stripWhitespace)

stripWhitespace = reverse.(dropWhile (==' ')).reverse

headersL line | hl == 0  = line
              | hl == 1  = "\\section*{" ++ header ++ "}"
              | hl == 2  = "\\subsection*{" ++ header ++ "}"
              | hl == 3  = "\\subsubsection*{" ++ header ++ "}"
              | hl >  3  = "\\paragraph{" ++ header ++ "}"
	where (hl, header) = parseHeader line

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

listsL = groupLines (isPrefixOf "*") ((["\\begin{enumerate}"] ++).(++ ["\\end{enumerate}"]).(map (("\\item "++).tail)))
parasL = groupLines isJustText       ( ++[""])
prefoL = groupLines (isPrefixOf " ") ((["\\begin{verbatim}"] ++).(++ ["\\end{verbatim}"]).(map tail))

lists = groupLines (isPrefixOf "*") ((tagL "ul").(map ((tag "li").tail)))
paras = groupLines isJustText       ( tagL "p")
prefo = groupLines (isPrefixOf " ") ((tagL "pre").(map tail))

isJustText l = not (isPrefixOf "<" l) &&
	       not (null l)

words' text = a : cont
	where (a,b) = span (isAlphaNum) text     
	      cont | null b    = []
	           | otherwise = [head b] : words' (tail b)

camelCase  wi w = if isCamelCase w then linkPage  wi w else w
camelCaseL wi w = if isCamelCase w then linkPageL wi w else w
isCamelCase []      = False
isCamelCase (w:ord) = isUpper w && any isUpper ord && any isLower ord && all isAlphaNum (w:ord)

linkPageL wi a | a `elem` pagenames wi = (linkPageExt ext a) ++ more
              | otherwise             = "\\href{cgi/edit/"++a++"}{"++a++" (new)}"
 where linkPageExt ext txt = "\\href{"++ a ++"." ++ ext ++ "}{"++txt++"}"
       (ext:exts) = triple3 $ head $ filter ((==a).triple1) (sitemap wi)
       more | null exts  = ""
            | otherwise  = " ("++(concat $ intersperse ", " $ map (\e -> linkPageExt e e) exts)++")"

linkPage wi a | a `elem` pagenames wi = (linkPageExt ext a) ++ more
              | otherwise             = aHref ("cgi/edit/"++a) (a++" (new)")
 where linkPageExt ext txt = aHref (a ++"." ++ ext) txt
       (ext:exts) = triple3 $ head $ filter ((==a).triple1) (sitemap wi)
       more | null exts  = ""
            | otherwise  = " ("++(concat $ intersperse ", " $ map (\e -> linkPageExt e e) exts)++")"

links  wi = linksG wi linkPage  camelCase
linksL wi = linksG wi linkPageL camelCaseL

linksG wi linkPageF camelCaseF = concat.(links').words'
  where links' [] = []
	links' ("[":rest )  | null after                                  = "["               : links' rest
	                    | isValidPagename link && (head after) == "]" = linkPageF wi link : links' (tail after)
	                    | otherwise                                   = "["               : links' rest
	  where (linkParts,after) = span (/="]") rest
	        link = concat linkParts
	links' (w1:rest) = camelCaseF wi w1 : links' rest

isValidPagename = all (\c -> isAlphaNum c || c `elem` "_-/" ) 

specials formatRCF wi []       = []
specials formatRCF wi (line:r) | "!!" `encloses` line = (case map toLower $ takeout "!!" line of
				  		"hello"   -> ["Hello World"]
						-- The next line is cool.
						"sitemap" -> map ("* ["\/"]") $ sort $ pagenames wi
						"recentchanges" -> formatRCF (recentChanges wi)
						huh       -> ["Unknown Command \""++huh++"\""] 
				           ) ++ specials formatRCF wi r
	 	      | otherwise            = line : specials formatRCF wi r

formatRC = tagLP "ol" [("id","recentChanges")] . concatMap formatChange
  where	formatChange entry = tagL "li" $ tagL "table" $
                             map (tag "tr") $ map (\(a,b) -> tag "th" a ++ tag "td" b ) [ 
  		("Revision:",show (revision entry)),
  		("Author:"  ,      author   entry ),
  		("Date:",          date   entry ),
  		("Message:",      (tag "p" $ message entry )),
  		("Changed Files:",(tag "ul" $ concatMap (tag "li" . ("["\/"]"). pagename) (paths entry) ) )
		]

formatRCL = envLL "enumerate" . concatMap formatChange
  where	formatChange entry = (["\\item"]++) $ envLL "description" $
                             map (\(a,b) -> "\\item["++a ++"] "++ b ) [ 
  		("Revision:",show (revision entry)),
  		("Author:"  ,      author   entry ),
  		("Date:",          date   entry ),
  		("Message:",       message entry ),
  		("Changed Files:",(envL "itemize" $ concatMap (("\\item "++) . ("["\/"]") . pagename) (paths entry)))
		]


encloses sub str = sub `isPrefixOf` str && sub `isSuffixOf` str && length str > 2 * length sub
takeout  sub    = (drop (length sub)).reverse.(drop (length sub)).reverse
(\/) pre post str = pre ++ str ++ post
