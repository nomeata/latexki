module WikiData where

import qualified Data.ByteString.Lazy.Char8 as B
import System.Time


newtype PageName = PageName String deriving (Eq, Ord)
instance Show PageName where
 show (PageName pn) = "Page " ++ pn

data PageInfo = PageInfo {
	smPageName  :: PageName,
	smType      :: String,
	smContent   :: B.ByteString,
	smModTime   :: ClockTime
}

type RecentChanges = [LogEntry]
type RawRecentChanges = [RawLogEntry]

data RawLogEntry = RawLogEntry { revisionR :: Int, authorR :: String, dateR :: String, pathsR :: [String], messageR :: String }  -- deriving Show

data LogEntry = LogEntry { revision :: Int, author :: String, date :: String, links :: [Link], message :: InlineText } --  deriving Show

data Link = WikiLink PageInfo String | NewLink PageInfo | DLLink String | PlainLink String String -- deriving Show

data InlineTextElem = Text String | LinkElem Link | Image String String -- deriving Show

type InlineText = [InlineTextElem] 

data DocElement = Paragraph InlineText | EnumList [InlineText] | 
                  ItemList [InlineText] | PreFormat String |
		  Header Int String | RCElem RecentChanges | HLine | -- deriving Show
		  PreFormatBS B.ByteString 
		

type Document = [DocElement] 
