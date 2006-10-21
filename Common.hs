module Common (
	Dependency(FileDep,FileList,RepositoryChanges),

	FileProcessor,
	DepCalculator,

	RecentChanges,
	LogEntry(LogEntry),
	revision,
	author,
	date,
	paths,
	message,

	WikiInfo(WikiInfo),
	sitemap,
	wikiConfig,
	debug,
	debugLn,
	mainTitle,
	basenames,
	outputs,
	recentChanges,

	logfilename,
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

-- Dependency Datatype
data Dependency = FileDep FilePath | FileList | RepositoryChanges

type FileProcessor = FilePath -> WikiInfo -> IO ()
type DepCalculator = FilePath -> WikiInfo -> IO [Dependency]

type RecentChanges = [LogEntry]

data LogEntry = LogEntry { revision :: Int, author :: String, date :: String, paths :: [FilePath], message :: String } deriving (Show)

-- Data type
data WikiInfo = WikiInfo {	sitemap :: [(String, String, [String]) ],
				wikiConfig :: [(String,String)],
				recentChanges :: RecentChanges,
				debug :: String -> IO (),
				debugLn :: String -> IO ()
			}
mainTitle = (fromMaybe "A Wiki").(lookup "title").wikiConfig

basenames wi = map triple1 (sitemap wi)

outputs wi = concatMap ( \(b,_,exts) -> map ((b++".")++) exts) (sitemap wi)

logfilename = "latexki-run.log"
datadir     = "./data/"

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
