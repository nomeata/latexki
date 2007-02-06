module Common (
	FileProducer,
	runFileProducer,
	runFileProducers,
	producedFile,
	producedFiles,
	getTime,
	doesExist,
	getWi,
	liftIO,
	FileProcessor,

	WikiInfo(..),
	pagenames,
	getSiteMap,
	getWikiConfig,
	getMainTitle,
	getPagenames,
	getOutputs,
	getRecentChanges,
	getExistingOutput,

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
	editLink,
	editLinkLines,
	filename,
	dirname,
	safeChdir,
	writeFileSafe,
	safeRemoveFile,

	directoryFiles,
	recursiveFiles,

	replace,
	subListOf,
) where

import Maybe
import Monad
import List
import System.Directory
import System.Time
import Control.Monad.Writer
import Control.Monad.Reader
import CacheT

import qualified FilePath as FP

import WikiData

-- File Processor Datatype

type FileProducer a = WriterT [FilePath] (
	ReaderT WikiInfo (
		CacheT FilePath ClockTime (
			CacheT FilePath Bool ( IO )
			)
		)
	) a
type FileProcessor = FilePath -> FileProducer ()

runFileProducer :: WikiInfo -> FileProducer () -> IO [FilePath]
runFileProducer info producer =
	runCacheT doesFileExist (
		runCacheT getModificationTime' (
			runReaderT (
				execWriterT producer
			) info
		)
	)

runFileProducers :: WikiInfo -> [FileProducer ()] -> IO [FilePath]
runFileProducers info = runFileProducer info . sequence_

producedFiles :: [FilePath] -> FileProducer ()
producedFiles = tell

producedFile :: FilePath -> FileProducer ()
producedFile = producedFiles . (:[])

doesExist file = lift $ lift $ lift $ callCache file

getModificationTime' file = lift $ do
	ex <- doesFileExist file
	if ex then getModificationTime file
	      else return $ TOD 0 0

getTime :: FilePath -> FileProducer ClockTime
getTime file = lift $ lift $ callCache file

getWi :: FileProducer WikiInfo
getWi = ask

type PageName = String
type SiteMap = [(PageName, String, [String])]

-- General Info Data type
data WikiInfo = WikiInfo {	sitemap :: SiteMap,
				wikiConfig :: [(String,String)],
				recentChanges :: RawRecentChanges,
				existingOutput :: [FilePath]
			}

getSiteMap = getWi >>= return . sitemap
getWikiConfig = getWi >>= return  . wikiConfig
getRecentChanges = getWi >>= return . recentChanges
getExistingOutput = existingOutput `fmap` getWi

getMainTitle = getWikiConfig >>= return . fromMaybe "A Wiki" . lookup "title"

pagenames = map triple1 . sitemap
getPagenames = getWi >>= return . pagenames

getOutputs = getSiteMap >>= return . concatMap ( \(b,_,exts) -> map ((b++".")++) exts)

logfilename = "./latexki-run.log"
datadir     = "./data/"

safeChdir dir = createDirectoryIfMissing True dir >> setCurrentDirectory dir
writeFileSafe file str = createDirectoryIfMissing True (dirname file) >> writeFile file str
safeRemoveFile file = do exists <- doesFileExist file
                         if exists then removeFile file else return ()

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

editLink      page         = backDir page ++ "cgi/edit/" ++ page
editLinkLines page von bis = backDir page ++ "cgi/edit/" ++ page ++
				"?lines=" ++ show von ++ "-" ++ show bis

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
