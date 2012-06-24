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
instance Show PageInfo where
 show p1 = "PageInfo " ++ show (smPageName p1)

data LectureInfo = LectureInfo {
    liName     :: B.ByteString,
    liLecturer :: Maybe B.ByteString,
    liSemester :: Maybe B.ByteString,
    liMainFile :: PageInfo,
    liPDFData  :: Maybe PDFData
    }
    deriving Show

data PDFIndex = PDFIndex { pdfIndexTitle :: String, pdfIndexPage :: Int, pdfIndexSub :: [PDFIndex] } deriving (Show)
data PDFData = PDFData { numberOfPages :: Int, pdfIndex :: [PDFIndex] } deriving (Show)

data TexIndex = TexIndex {
    tiTitle :: B.ByteString,
    tiPageFrom :: Int,
    tiPageTo :: Int}
    deriving (Show)

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
	message :: InlineText,
	websvn :: Maybe Link }  deriving Show

data Link =	WikiLink PageInfo B.ByteString |
		NewLink String |
		DLLink B.ByteString |
		PlainLink B.ByteString B.ByteString deriving Show

data InlineTextElem =	Text B.ByteString |
			LinkElem Link |
			Image B.ByteString B.ByteString deriving Show

type InlineText = [InlineTextElem] 

data DocElement = Paragraph InlineText | EnumList [InlineText] | 
                  ItemList [InlineText] | PreFormat B.ByteString |
		  Header Int B.ByteString | RCElem RecentChanges | 
                  LIElem LectureInfo |
                  HLine
    deriving Show
		

type Document = [DocElement]  
