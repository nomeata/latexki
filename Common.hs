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
	mainTitle,
	pagenames,
	outputs,
	recentChanges,

	logfilename,
	datadir,

	triple1,
	triple2,
	triple3,

	pagename,
	splitFilePath,
	splitWikiPath,
	dirTrail,
	backDir,
	filename,
	dirname,
	safeChdir,
	writeFileSafe,

	directoryFiles,
	recursiveFiles,

	replace,
	subListOf,
) where

import qualified FilePath as FP
import Maybe
import Monad
import List

import System.Directory

-- Dependency Datatype
data Dependency = FileDep FilePath | FileList | RepositoryChanges

type FileProcessor = FilePath -> WikiInfo -> IO ()
type DepCalculator = FilePath -> WikiInfo -> IO [Dependency]

type RecentChanges = [LogEntry]

data LogEntry = LogEntry { revision :: Int, author :: String, date :: String, paths :: [FilePath], message :: String } deriving (Show)

type PageName = String

-- Data type
data WikiInfo = WikiInfo {	sitemap :: [(PageName, String, [String]) ],
				wikiConfig :: [(String,String)],
				recentChanges :: RecentChanges
			}
mainTitle = (fromMaybe "A Wiki").(lookup "title").wikiConfig

pagenames wi = map triple1 (sitemap wi)

outputs wi = concatMap ( \(b,_,exts) -> map ((b++".")++) exts) (sitemap wi)

logfilename = "./latexki-run.log"
datadir     = "./data/"

safeChdir dir = createDirectoryIfMissing True dir >> setCurrentDirectory dir
writeFileSafe file str = createDirectoryIfMissing True (dirname file) >> writeFile file str

pagename = removeExt . fst . splitWikiPath
filename = snd.FP.splitFileName 
dirname  = fst.FP.splitFileName

removeExt = takeWhile (/='.')

splitWikiPath :: FilePath -> (PageName, String)
splitWikiPath path' = case break (== '.') path of
    (name, "")    -> (name, "")
    (name, _:ext) -> (name, ext)
  where	path = stripPrefix "/" $ stripPrefix datadir path'

splitFilePath :: FilePath -> (String, String, String)
splitFilePath path = case break (== '.') basename of
    (name, "")      -> (dir, name, "")
    (name, _:ext) -> (dir, name, ext)
  where
    (dir, basename) = FP.splitFileName path

stripPrefix pre str | pre `isPrefixOf` str = drop (length pre) str
                    | otherwise            = str

dirTrail :: FilePath -> [FilePath]
dirTrail = map reverse . dirTrail' . reverse
dirTrail' "" = []
dirTrail' ('/':dir) = dir : dirTrail' dir
dirTrail' path = dirTrail' $ dropWhile (/='/')  path

backDir path =  concat $ replicate (length $ filter (=='/') path) "../"

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

directoryFiles dir = getDirectoryContents dir >>= return.(map (dir++)) >>= filterM (doesFileExist)

recursiveFiles :: FilePath -> IO [FilePath]
recursiveFiles dir' = do
	let dir = if last dir' == '/' then dir' else dir'++"/"
	entries <- getDirectoryContents dir
	let paths = map (dir++) $ filter ( (/='.').head ) entries
	files  <- filterM doesFileExist paths
	recurs <- liftM concat $ mapM recursiveFiles =<< filterM doesDirectoryExist paths
	return $ files ++ recurs
