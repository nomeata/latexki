module SVN (getSVNRecentChanges,updateSVN,coSVN) where

import System
import System.Process
import System.IO
import Control.Concurrent

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Types
import Text.XML.HaXml.Verbatim

import WikiData

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
	xml <- hGetContents out
        forkIO $ waitForProcess pid >> return ()
	let (Document _ _ info _)= xmlParse "svn info" xml
	return $ read $ verbatim $ find "revision" literal `o` tagWith (=="entry") `o` children $ CElem info


getSVNRecentChanges repos = do 
	let options = ["log","--xml","--limit","10","--verbose",datadir]
	(inp,out,err,pid) <- runInteractiveProcess "svn" options Nothing Nothing
	hClose inp
	xml <- hGetContents out
        forkIO $ waitForProcess pid >> return ()
	let doc= xmlParse "svn log" xml
	return $ toLogEntries doc

toLogEntries (Document _ _ logs _ ) = map toLogEntry $ elm `o` children $ CElem logs

toLogEntry entry = RawLogEntry revision author date paths message
  where	getElem name = verbatim $ txt `o` children `o` tagWith (==name) `o` children $ entry
  	revision = read $ verbatim $ find "revision" literal $ entry
	author	 = getElem "author"
	date	 = getElem "date"
	paths	 = map verbatim $ txt `o` children `o` tagWith (=="path") `o`
				          children `o` tagWith (=="paths") `o` children $ entry
	message	 = getElem "msg"
	

