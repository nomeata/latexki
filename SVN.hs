module SVN (getSVNRecentChanges,getSVNLastChange,updateSVN,coSVN) where

import System.Process
import System.IO
import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as B
import System.FilePath
import Data.Time
import System.Locale

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Verbatim

import qualified Data.ByteString.Lazy.UTF8 as UTF8

import WikiData hiding (Document)


{-
  For later specification of the kind of deleted file
 -data ChangedPath = Added FilePath | Deleted FilePath | Modified FilePath | Other FilePath
-}

import Common

updateSVN repos = system ("(cd "++datadir++"; svn update)") >> return ()

coSVN repos = system ("svn checkout "++repos++" "++datadir) >> return ()

getCurrentRev repos = do
	let options = ["info","--xml", datadir]
	(inp,out,err,pid) <- runInteractiveProcess "svn" options Nothing Nothing
	hClose inp
        hSetEncoding out utf8
	xml <- hGetContents out
	-- this fails in instances
        forkIO $ waitForProcess pid >> return ()
	let (Document _ _ info _)= xmlParse "svn info" xml
	return $ read $ verbatim $ find "revision" literal `o` tagWith (=="entry") `o` children $ CElem info noPos


getSVNLastChange repos file = do 
	let options = ["log","--xml","--limit","1","--verbose",datadir </> file]
	(inp,out,err,pid) <- runInteractiveProcess "svn" options Nothing Nothing
	hClose inp
        hSetEncoding out utf8
	xml <- hGetContents out
	if xml /= xml then return () else return ()
        forkIO $ waitForProcess pid >> return ()
	let doc= xmlParse "svn log" xml
        tz <- getCurrentTimeZone
	return $ head $ toLogEntries tz doc

getSVNRecentChanges repos = do 
	let options = ["log","--xml","--limit","10","--verbose",datadir]
	(inp,out,err,pid) <- runInteractiveProcess "svn" options Nothing Nothing
	hClose inp
        hSetEncoding out utf8
	xml <- hGetContents out
	if xml /= xml then return () else return ()
        forkIO $ waitForProcess pid >> return ()
	let doc= xmlParse "svn log" xml
        tz <- getCurrentTimeZone
	return $ toLogEntries tz doc

toLogEntries :: TimeZone -> Document Posn -> [RawLogEntry]
toLogEntries tz (Document _ _ logs _ ) = map (toLogEntry tz) $ elm `o` children $ CElem logs
 noPos
toLogEntry tz entry = RawLogEntry revision author date paths message
  where	getElem name = verbatim $ txt `o` children `o` tagWith (==name) `o` children $ entry
  	revision = read $ verbatim $ find "revision" literal $ entry
	author	 = UTF8.fromString $ getElem "author"
	date	 = utcToZonedTime tz $ readTime defaultTimeLocale svnTimeFormat (getElem "date")
	paths	 = map (tail.verbatim) $ txt `o` children `o` tagWith (=="path") `o`
			 	         	 children `o` tagWith (=="paths") `o` children $ entry
	message	 = UTF8.fromString $ getElem "msg"
	

svnTimeFormat = "%Y-%m-%dT%H:%M:%S%Q%Z"
