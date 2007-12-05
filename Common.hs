module Common (
	FileProducer,
	runFileProducer,
	runFileProducers,
	producedFile,
	producedFiles,
	getWi,
	liftIO,
	FileProcessor,

	getInputTime,
	getOutputTime,
	doesOutputExist,

	outputFile,

	de2sm,
	pagename,
	pageOutput,
	pageVariant,
	pageOutputs,
	pageInput,
	pageType,
	pageSource,

	WikiInfo(..),
	getSiteMap,
	getWikiConfig,
	getMainTitle,
	getOutputs,
	getRecentChanges,
	getExistingOutput,
	pageExts,
	lookupPage,

	logfilename,
	datadir,

	triple1,
	triple2,
	triple3,

	dirTrail,
	backDir,
	editLink,
	editLinkLines,
	namedNewLink,
	newLink,
	inTargetDir,
	inDir,
	fileRelative,
	writeFileSafe,
	appendFileSafe,
	openFileSafe,
	safeRemoveFile,

	directoryFiles,
	recursiveFiles,

	replaceBS,
	subListOf,
) where

import Maybe
import Monad
import List
import System.Directory
import System.Time
import System.IO
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as B
import System.FilePath

import CacheT

import ReadDir
import WikiData

-- What produces what

generated_by :: String -> [String]
generated_by "tex"   = ["html","pdf","png"]
generated_by "latex" = generated_by "tex"
generated_by ""      = ["html","pdf"]
generated_by "css"   = ["html","css"]
generated_by "png"   = ["html","pdf"]
generated_by "jpg"   = ["html","pdf"]
generated_by "eps"   = ["html"]
generated_by _       = ["html"]


-- File Processor Datatype

type FileProducer a = WriterT [FilePath] (
	ReaderT WikiInfo (
--		CacheT FilePath ClockTime (
--			CacheT FilePath Bool (
				IO
--			)
		)
	) a
type FileProcessor = PageInfo -> FileProducer ( [ ([FilePath], FileProducer()) ] )

runFileProducer :: WikiInfo -> FileProducer () -> IO [FilePath]
runFileProducer info producer =
--	runCacheT doesFileExist (
--		runCacheT getModificationTime' (
			runReaderT (
				execWriterT producer
			) info
--		)
--	)

runFileProducers :: WikiInfo -> [FileProducer ()] -> IO [FilePath]
runFileProducers info = runFileProducer info . sequence_

producedFiles :: [FilePath] -> FileProducer ()
producedFiles = tell

producedFile :: FilePath -> FileProducer ()
producedFile = producedFiles . (:[])

-- doesExist file = lift $ lift $ lift $ callCache file

--getModificationTime' file = lift $ do
--	ex <- doesFileExist file
--	if ex then getModificationTime file
--	      else return $ TOD 0 0

--getTime :: FilePath -> FileProducer ClockTime
--getTime file = lift $ lift $ callCache file

getOutputTime file = (fmap deModTime . listToMaybe . filter ((file ==) . deFileName)) `liftM` getExistingOutput 
doesOutputExist file = (not . null  . filter ((file ==) . deFileName)) `liftM` getExistingOutput 

getInputTime = smModTime 
--doesInputExist page = isJust `liftM` lookupPage page

getWi :: FileProducer WikiInfo
getWi = ask

--newtype PageName = PageName String deriving (Eq, Ord)
--instance Show PageName where
-- show (PageName pn) = "Page " ++ pn


pagename PageInfo {smPageName = PageName pn} = pn

de2sm de = PageInfo {
		smPageName = PageName (dropExtensions (deFileName de)),
		smType     = dropWhile (=='.') $ takeExtensions (deFileName de),
		smModTime  = deModTime de,
		smContent  = deFileContent de
		}

type SiteMap = [PageInfo]


-- General Info Data type
data WikiInfo = WikiInfo {	sitemap :: SiteMap,
				wikiConfig :: [(String,String)],
				recentChanges :: RawRecentChanges,
				existingOutput :: [DirEntry]
			}

getSiteMap = sitemap `liftM` getWi
getWikiConfig = wikiConfig `liftM` getWi
getRecentChanges = recentChanges `liftM` getWi 
getExistingOutput = existingOutput `liftM` getWi

getMainTitle = getWikiConfig >>= return . fromMaybe "A Wiki" . lookup "title"

getOutputs = concatMap pageOutputs `liftM` getSiteMap 

pageSource page = smContent page
pageType page = smType page
pageOutput PageInfo {smPageName = pn, smType = t} ext =      outputFile pn ext 
pageVariant PageInfo {smPageName = pn, smType = t} ext =     outputVariant pn ext 
pageOutputs PageInfo {smPageName = pn, smType = t}    = map (outputFile pn) $ generated_by t 
pageInput PageInfo {smPageName = pn, smType = t}      = datadir </> outputFile pn t

outputFile    (PageName base) ext = base <.> ext
outputVariant (PageName base) ext = base ++ "-" ++ ext

inDir dir action = do
	if not (null dir) then  do
		cwd <- liftIO $ getCurrentDirectory
		liftIO $ print $ "Changing Dir to "++dir
		liftIO $ safeChdir dir
		ret <- action
		liftIO $ print $ "Changing Dir to "++cwd
		liftIO $ setCurrentDirectory cwd
		return ret
	  else  action

inTargetDir page  = inDir $ takeDirectory (pagename page)

lookupPage page = listToMaybe . filter ((page ==) . smPageName) 

pageExts page =  generated_by $ smType page


logfilename = "latexki-run.log"
datadir     = "data"

safeChdir dir = createDirectoryIfMissing True dir >> setCurrentDirectory dir
writeFileSafe file str = do
	createDirectoryIfMissing True (takeDirectory file)
	B.writeFile file str
appendFileSafe file str = do
	createDirectoryIfMissing True (takeDirectory file)
	appendFile file str
openFileSafe file mode = do
	createDirectoryIfMissing True (takeDirectory file)
	openFile file mode

safeRemoveFile file = do exists <- doesFileExist file
                         if exists then removeFile file else return ()

--filename = takeExtensions
--dirname  = takeDirectory


stripPrefix pre str | pre `isPrefixOf` str = drop (length pre) str
                    | otherwise            = str

dirTrail :: FilePath -> [FilePath]
dirTrail = map reverse . dirTrail' . reverse
dirTrail' "" = []
dirTrail' ('/':dir) = dir : dirTrail' dir
dirTrail' path = dirTrail' $ dropWhile (/='/')  path

--backDir' path =  concat $ replicate (length $ filter (=='/') path) "../"
backDir' path = joinPath $ replicate (length (splitPath path) - 1) ".."
backDir  page = backDir' (pagename page)

editLink page  =  "cgi/edit" </> pagename page
namedNewLink page  =  "cgi/edit" </> page
newLink =  "cgi/edit"
fileRelative :: PageInfo -> String
fileRelative page = backDir page </> pageInput page

editLinkLines page from to  = backDir page </> "cgi/edit/" ++ (pagename page) ++ "?lines=" ++ show from ++ "-" ++ show to

triple1 (x,_,_) = x
triple2 (_,x,_) = x
triple3 (_,_,x) = x
		

replaceBS what with txt = fromMaybe txt $ listToMaybe $ catMaybes $ map replaceIfFoundAt $ B.elemIndices (B.head what) txt
  where replaceIfFoundAt n | what `B.isPrefixOf` B.drop n txt = Just $ B.take n txt `B.append` with `B.append` B.drop (n+ B.length what) txt
                           | otherwise                      = Nothing


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
