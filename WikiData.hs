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
instance Eq PageInfo where
 p1 == p2 = smPageName p1 == smPageName p2
instance Ord PageInfo where
 p1 `compare` p2 = smPageName p1 `compare` smPageName p2

type RecentChanges = [LogEntry]
type RawRecentChanges = [RawLogEntry]

data RawLogEntry = RawLogEntry {
	revisionR :: Int,
	authorR :: B.ByteString,
	dateR :: B.ByteString,
	pathsR :: [String],
	messageR :: B.ByteString
	}  -- deriving Show

data LogEntry = LogEntry {
	revision :: Int,
	author :: B.ByteString,
	date :: B.ByteString,
	links :: [Link],
	message :: InlineText } --  deriving Show

data Link =	WikiLink PageInfo B.ByteString |
		NewLink String |
		DLLink B.ByteString |
		PlainLink B.ByteString B.ByteString -- deriving Show

data InlineTextElem =	Text B.ByteString |
			LinkElem Link |
			Image B.ByteString B.ByteString -- deriving Show

type InlineText = [InlineTextElem] 

data DocElement = Paragraph InlineText | EnumList [InlineText] | 
                  ItemList [InlineText] | PreFormat B.ByteString |
		  Header Int B.ByteString | RCElem RecentChanges | HLine  -- deriving Show
		

type Document = [DocElement] 
