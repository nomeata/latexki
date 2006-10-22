module SVN (getSVNRecentChanges,updateSVN,coSVN) where

import System
import System.Process
import System.IO

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Types
import Text.XML.HaXml.Verbatim

data ChangedPath = Added file | Deleted file | Modified file | Other file

import Common

updateSVN repos = system ("(cd "++datadir++"; svn update)") >> return ()

coSVN repos = system ("svn checkout "++repos++" "++datadir) >> return ()

getCurrentRev repos = do
	let options = ["info","--xml", datadir]
	(inp,out,err,pid) <- runInteractiveProcess "svn" options Nothing Nothing
	hClose inp
	xml <- hGetContents out
	waitForProcess pid
	let (Document _ _ info _)= xmlParse "svn info" xml
	return $ read $ verbatim $ find "revision" literal `o` tagWith (=="entry") `o` children $ CElem info


getSVNRecentChanges repos = do 
	let options = ["log","--xml","--limit","10","--verbose",datadir]
	(inp,out,err,pid) <- runInteractiveProcess "svn" options Nothing Nothing
	hClose inp
	xml <- hGetContents out
	--no deadlock please
	--waitForProcess pid
	let doc= xmlParse "svn log" xml
	return $ toLogEntries doc

toLogEntries (Document _ _ logs _ ) = map toLogEntry $ elm `o` children $ CElem logs

toLogEntry entry = LogEntry revision author date paths message
  where	getElem name = verbatim $ txt `o` children `o` tagWith (==name) `o` children $ entry
  	revision = read $ verbatim $ find "revision" literal $ entry
	author	 = getElem "author"
	date	 = getElem "date"
	paths	 = map verbatim $ txt `o` children `o` tagWith (=="path") `o`
				          children `o` tagWith (=="paths") `o` children $ entry
	message	 = getElem "msg"
	

