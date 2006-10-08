module Common (
	FileProcessor,
	DepCalculator,

	WikiInfo(WikiInfo),
	sitemap,
	wikiConfig,
	mainTitle,
	basenames,
	outputs,

	datadir,

	triple1,
	triple2,
	triple3,

	basename,
	splitFilePath,
	filename,

	replace,
	subListOf,
) where

import qualified FilePath as FP
import Maybe
import List

-- Data type

type FileProcessor = FilePath -> WikiInfo -> IO ()
type DepCalculator = FilePath -> WikiInfo -> IO [FilePath]

data WikiInfo = WikiInfo { sitemap :: [(String, String, [String]) ] , wikiConfig :: [(String,String)] }
mainTitle = (fromMaybe "A Wiki").(lookup "title").wikiConfig

basenames wi = map triple1 (sitemap wi)

outputs wi = concatMap ( \(b,_,exts) -> map ((b++".")++) exts) (sitemap wi)

datadir = "./data/"

basename = triple2.splitFilePath
filename = snd.FP.splitFileName 
splitFilePath :: FilePath -> (String, String, String)
splitFilePath path = case break (== '.') basename of
    (name, "")      -> (dir, name, "")
    (name, _:ext) -> (dir, name, ext)
  where
    (dir, basename) = FP.splitFileName path

triple1 (x,_,_) = x
triple2 (_,x,_) = x
triple3 (_,_,x) = x
		
replace what with l = replace' l
 where 	replace' []   = []
	replace' text = if what `isPrefixOf` text then with ++ (replace' (drop (length what) text) )
						  else head text : replace' (tail text)

subListOf []   _   = True
subListOf what l = contains' l
 where 	contains' []   = False
	contains' text = what `isPrefixOf` text || contains' (tail text)
